use darling::{Error, FromMeta};
use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use rocket_http::{ext::IntoOwned, uri::Origin, MediaType, Method};
use std::str::FromStr;
use syn::spanned::Spanned;
use syn::{Attribute, Meta, MetaList, NestedMeta};

#[derive(Debug)]
pub struct Route {
    pub method: Method,
    pub origin: Origin<'static>,
    pub media_type: Option<MediaType>,
    pub data_param: Option<String>,
    pub doc_string: Option<String>,
}

impl Route {
    pub fn path_params(&self) -> impl Iterator<Item = &str> {
        self.origin.segments().filter_map(|s| {
            if s.starts_with('<') && s.ends_with('>') && !s.ends_with("..>") {
                Some(&s[1..s.len() - 1])
            } else {
                None
            }
        })
    }

    pub fn _path_multi_param(&self) -> Option<&str> {
        self.origin.segments().find_map(|s| {
            if s.starts_with('<') && s.ends_with("..>") {
                Some(&s[1..s.len() - 3])
            } else {
                None
            }
        })
    }
}

#[derive(Debug)]
struct OriginMeta(Origin<'static>);
#[derive(Debug)]
struct MediaTypeMeta(MediaType);
#[derive(Debug)]
struct MethodMeta(Method);

impl FromMeta for OriginMeta {
    fn from_string(value: &str) -> Result<Self, Error> {
        match Origin::parse_route(value) {
            Ok(o) => Ok(OriginMeta(o.into_owned())),
            Err(e) => Err(Error::unsupported_format(&e.to_string())),
        }
    }
}

impl FromMeta for MediaTypeMeta {
    fn from_string(value: &str) -> Result<Self, Error> {
        match MediaType::parse_flexible(value) {
            Some(m) => Ok(MediaTypeMeta(m)),
            None => Err(Error::unsupported_format(&format!(
                "Unknown media type: '{}'",
                value
            ))),
        }
    }
}

impl FromMeta for MethodMeta {
    fn from_string(value: &str) -> Result<Self, Error> {
        match Method::from_str(value) {
            Ok(m) => Ok(MethodMeta(m)),
            Err(()) => Err(Error::unsupported_format(&format!(
                "Unknown HTTP method: '{}'",
                value
            ))),
        }
    }
}

#[derive(Debug, FromMeta)]
#[darling(allow_unknown_fields)]
struct RouteAttributeNamedMeta {
    path: OriginMeta,
    #[darling(default)]
    format: Option<MediaTypeMeta>,
    #[darling(default)]
    data: Option<String>,
}

#[derive(Debug, FromMeta)]
#[darling(allow_unknown_fields)]
struct MethodRouteAttributeNamedMeta {
    #[darling(default)]
    format: Option<MediaTypeMeta>,
    #[darling(default)]
    data: Option<String>,
}

fn parse_route_attr(args: &[NestedMeta], doc_string: Option<String>) -> Result<Route, Error> {
    if args.is_empty() {
        return Err(Error::too_few_items(1));
    }
    let method = MethodMeta::from_nested_meta(&args[0])?;
    let named = RouteAttributeNamedMeta::from_list(&args[1..])?;
    Ok(Route {
        method: method.0,
        origin: named.path.0,
        media_type: named.format.map(|x| x.0),
        data_param: named.data.map(trim_angle_brackers),
        doc_string,
    })
}

fn parse_method_route_attr(
    method: Method,
    args: &[NestedMeta],
    doc_string: Option<String>,
) -> Result<Route, Error> {
    if args.is_empty() {
        return Err(Error::too_few_items(1));
    }
    let origin = OriginMeta::from_nested_meta(&args[0])?;
    let named = MethodRouteAttributeNamedMeta::from_list(&args[1..])?;
    Ok(Route {
        method,
        origin: origin.0,
        media_type: named.format.map(|x| x.0),
        data_param: named.data.map(trim_angle_brackers),
        doc_string,
    })
}

fn trim_angle_brackers(mut s: String) -> String {
    if s.starts_with('<') && s.ends_with('>') {
        s.pop();
        s.remove(0);
    }
    s
}

fn parse_attr(name: &str, args: &[NestedMeta], doc_string: Option<String>) -> Result<Route, Error> {
    match Method::from_str(name) {
        Ok(method) => parse_method_route_attr(method, args, doc_string),
        Err(()) => parse_route_attr(args, doc_string),
    }
}

fn is_route_attribute(a: &Attribute) -> bool {
    a.path.is_ident("get")
        || a.path.is_ident("put")
        || a.path.is_ident("post")
        || a.path.is_ident("delete")
        || a.path.is_ident("options")
        || a.path.is_ident("head")
        || a.path.is_ident("trace")
        || a.path.is_ident("connect")
        || a.path.is_ident("patch")
        || a.path.is_ident("route")
}

fn to_name_and_args(attr: &Attribute) -> Option<(String, Vec<NestedMeta>)> {
    match attr.parse_meta() {
        Ok(Meta::List(MetaList { path, nested, .. })) => path
            .get_ident()
            .map(|name| (name.to_string(), nested.into_iter().collect())),
        _ => None,
    }
}

pub(crate) fn parse_attrs<'a>(attrs: &[Attribute]) -> Result<Route, TokenStream> {
    let doc_lines = attrs
        .iter()
        .filter(|attr| {
            let ident = match attr.path.get_ident() {
                Some(i) => i,
                None => return false,
            };
            // FIXME: is there a better way to check the ident type? This allocation makes me sad.
            ident.to_string() == "doc"
        })
        .filter_map(|attr| {
            // FIXME: fetch the literal without cloning the tokens.
            let mut iter = attr.tokens.clone().into_iter();
            iter.next(); // ignore punct
            match iter.next() {
                Some(TokenTree::Literal(lit)) => Some(lit),
                _ => None,
            }
        })
        .map(|literal| {
            // FIXME: can the lit be converted to a string without the quotes?
            let mut quoted_literal = literal.to_string();
            quoted_literal.remove(0);
            quoted_literal.pop();
            // drop optional but common whitespace in the beginning
            quoted_literal.trim_start().to_owned()
        })
        .collect::<Vec<_>>();

    let doc_string = if !doc_lines.is_empty() {
        Some(doc_lines.join("\n"))
    } else {
        None
    };

    let route_attr = attrs
        .into_iter()
        .find(|a| is_route_attribute(a))
        .ok_or_else(|| TokenStream::from(quote! {
            compile_error!("Could not find Rocket route attribute. Ensure the #[openapi] attribute is placed *before* the Rocket route attribute.");
        }))?;

    let span = route_attr.span();
    let (name, args) = to_name_and_args(&route_attr).ok_or_else(|| {
        TokenStream::from(quote_spanned! {span=>
            compile_error!("Malformed route attribute");
        })
    })?;

    parse_attr(&name, &args, doc_string).map_err(|e| e.with_span(&route_attr).write_errors().into())
}
