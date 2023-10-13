use std::str::FromStr;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{parse::Parser, punctuated::Punctuated, LitStr, Token};

#[derive(Debug)]
enum Method {
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    HEAD,
    OPTIONS,
}

impl TryFrom<&str> for Method {
    type Error = &'static str;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "get" | "GET" => Ok(Self::GET),
            "post" | "POST" => Ok(Self::POST),
            "put" | "PUT" => Ok(Self::PUT),
            "delete" | "DELETE" => Ok(Self::DELETE),
            "patch" | "PATCH" => Ok(Self::PATCH),
            "head" | "HEAD" => Ok(Self::HEAD),
            "options" | "OPTIONS" => Ok(Self::OPTIONS),
            _ => Err("unknown method"),
        }
    }
}

impl std::str::FromStr for Method {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::try_from(s)
    }
}

impl Method {
    fn as_str(&self) -> &'static str {
        match self {
            Self::GET => "get",
            Self::POST => "post",
            Self::PUT => "put",
            Self::DELETE => "delete",
            Self::PATCH => "patch",
            Self::HEAD => "head",
            Self::OPTIONS => "options",
        }
    }
}

/// parse attribute, get method, path, header
///
/// # Example
/// ```
/// #[get("/user/{id}")]
/// #[header("Content-Type", "application/json")]
/// fn get_user(&self, id: u32) -> Result<User, Error>;
/// ```
///
/// the attribute above will parse to
/// method: GET
/// path: "/user/{id}"
/// header: ("Content-Type", "application/json")
///
/// # spec
/// format is `#[cmd(arg)`, Outer attribute, Meta::List
/// which `cmd` can be http method or header
/// - method: get, post, put, delete, patch, head, options
/// - header: header
///
/// other cmd will be ignored.
///
/// when cmd is method, arg is path.
/// path can contain variable, which will be replaced by input argument.
/// path need to be a string literal, which means it should be wrapped by `""`.
///
/// when cmd is header, arg is header name and value.
/// name and value just be formed a string tuple.
///
#[derive(Debug)]
struct MethodAttrInfo {
    pub path: String,
    pub method: Method,
    pub header: Vec<(String, String)>,
}

impl TryFrom<&[syn::Attribute]> for MethodAttrInfo {
    type Error = syn::Error;

    fn try_from(value: &[syn::Attribute]) -> Result<Self, Self::Error> {
        let mut path = Option::<String>::None;
        let mut method = Option::<Method>::None;
        let mut header = Vec::new();

        for attr in value {
            if !matches!(attr.style, syn::AttrStyle::Outer) {
                continue;
            }

            match attr.meta {
                syn::Meta::List(ref l) => {
                    if let Some(idt) = l.path.get_ident() {
                        let cmd = idt.to_string();
                        if let Ok(mth) = <Method as std::str::FromStr>::from_str(cmd.as_str()) {
                            method = Some(mth);
                            if let Ok(arg) = l.parse_args::<LitStr>() {
                                path = Some(arg.value());
                            } else {
                                return Err(syn::Error::new_spanned(
                                    l,
                                    "path is not string literal",
                                ));
                            }
                        } else if cmd == "header" {
                            let parser = Punctuated::<LitStr, Token![,]>::parse_terminated;
                            if let Ok(punct) = parser.parse2(l.tokens.clone()) {
                                if punct.len() != 2 {
                                    return Err(syn::Error::new_spanned(
                                        l,
                                        "header format is #[header(name, value)]",
                                    ));
                                }
                                let key = punct.first().unwrap().value();
                                let value = punct.last().unwrap().value();
                                header.push((key, value));
                            } else {
                                return Err(syn::Error::new_spanned(
                                    l,
                                    "header format is #[header(name, value)]",
                                ));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(Self {
            method: method.ok_or(syn::Error::new_spanned(
                value.first().unwrap(),
                "method not found",
            ))?,
            path: path.ok_or(syn::Error::new_spanned(
                value.first().unwrap(),
                "path not found",
            ))?,
            header: header,
        })
    }
}

/// parse signature, get name from input
///
/// # Example
/// ```
/// fn exmaple(
///     &self,
///     #[qurey("name")] name: String,
///     #[query_map] query: QueryUser,
///     #[json] json: JsonUser,
///     #[form] form: FormUser,
///     #[path("id")] id: u32,
///     #[header("Content-Type")] content_type: String,
///     #[header_map] headers: HeaderUser
/// ) -> Result<(), Error>;
/// ```
///
/// now, arg just simple value, no pattern support.
///
#[derive(Debug)]
enum ArgAttrInfo {
    Query {
        query_name: String,
        arg_name: String,
    },
    QueryMap {
        arg_name: String,
    },
    Json {
        arg_name: String,
    },
    Form {
        arg_name: String,
    },
    Path {
        path_var_name: String,
        arg_name: String,
    },
    Header {
        header_name: String,
        arg_name: String,
    },
    HeaderMap {
        arg_name: String,
    },
}

impl TryFrom<&syn::PatType> for ArgAttrInfo {
    type Error = syn::Error;

    fn try_from(arg: &syn::PatType) -> Result<Self, Self::Error> {
        const ERR_MSG: &'static str = "arg attr just query, header_map, json, query_map etc.";

        if arg.attrs.len() != 1 {
            return Err(syn::Error::new_spanned(
                arg,
                "arg should have just one attribute",
            ));
        }

        let attr = arg.attrs.first().unwrap();

        if !matches!(attr.style, syn::AttrStyle::Outer) {
            return Err(syn::Error::new_spanned(
                attr,
                "arg attr not outer attribute",
            ));
        }

        let arg_name: syn::Result<String> = {
            match *arg.pat {
                syn::Pat::Ident(ref pi) => {
                    let arg_name = pi.ident.to_string();
                    Ok(arg_name)
                }
                _ => {
                    return Err(syn::Error::new_spanned(
                        arg.pat.clone(),
                        "arg need to be an ident",
                    ));
                }
            }
        };
        let arg_name = arg_name?;

        match attr.meta {
            syn::Meta::List(ref l) => {
                let ident = l.path.get_ident();
                if ident.is_none() {
                    return Err(syn::Error::new_spanned(l.path.clone(), ERR_MSG));
                }
                match ident.unwrap().to_string().as_str() {
                    "query" => {
                        let parser = Punctuated::<LitStr, Token![,]>::parse_terminated;
                        if let Ok(punct) = parser.parse2(l.tokens.clone()) {
                            if punct.len() != 1 {
                                return Err(syn::Error::new_spanned(
                                    l.tokens.clone(),
                                    "arg attr query format is #[query(name)]",
                                ));
                            }
                            let query_name = punct.first().unwrap().value();
                            return Ok(Self::Query {
                                query_name,
                                arg_name,
                            });
                        } else {
                            return Err(syn::Error::new_spanned(
                                l.tokens.clone(),
                                "arg attr query format is #[query(name)]",
                            ));
                        }
                    }
                    "path" => {
                        let parser = Punctuated::<LitStr, Token![,]>::parse_terminated;
                        if let Ok(punct) = parser.parse2(l.tokens.clone()) {
                            if punct.len() != 1 {
                                return Err(syn::Error::new_spanned(
                                    l.tokens.clone(),
                                    "arg attr path format is #[path(name)]",
                                ));
                            }
                            let path_var_name = punct.first().unwrap().value();
                            return Ok(Self::Path {
                                path_var_name,
                                arg_name,
                            });
                        } else {
                            return Err(syn::Error::new_spanned(
                                l.tokens.clone(),
                                "arg attr path format is #[path(name)]",
                            ));
                        }
                    }
                    "header" => {
                        let parser = Punctuated::<LitStr, Token![,]>::parse_terminated;
                        if let Ok(punct) = parser.parse2(l.tokens.clone()) {
                            if punct.len() != 1 {
                                return Err(syn::Error::new_spanned(
                                    l.tokens.clone(),
                                    "arg attr header format is #[header(name)]",
                                ));
                            }
                            let header_name = punct.first().unwrap().value();
                            return Ok(Self::Header {
                                header_name,
                                arg_name,
                            });
                        } else {
                            return Err(syn::Error::new_spanned(
                                l.tokens.clone(),
                                "arg attr header format is #[header(name)]",
                            ));
                        }
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(l.path.clone(), ERR_MSG));
                    }
                }
            }
            syn::Meta::Path(ref p) => {
                if let Some(ident) = p.get_ident() {
                    match ident.to_string().as_str() {
                        "query_map" => {
                            return Ok(Self::QueryMap { arg_name: arg_name });
                        }
                        "json" => {
                            return Ok(Self::Json { arg_name: arg_name });
                        }
                        "form" => {
                            return Ok(Self::Form { arg_name: arg_name });
                        }
                        "header_map" => {
                            return Ok(Self::HeaderMap { arg_name: arg_name });
                        }
                        _ => {
                            return Err(syn::Error::new_spanned(p, ERR_MSG));
                        }
                    }
                } else {
                    return Err(syn::Error::new_spanned(p, ERR_MSG));
                }
            }
            _ => {
                return Err(syn::Error::new_spanned(attr, ERR_MSG));
            }
        }
    }
}

struct MethodInfo {
    item: syn::TraitItemFn,
    attr: MethodAttrInfo,
    args: Vec<ArgAttrInfo>,
}

impl TryFrom<syn::TraitItemFn> for MethodInfo {
    type Error = syn::Error;

    fn try_from(mut value: syn::TraitItemFn) -> Result<Self, Self::Error> {
        if value.sig.asyncness.is_none() {
            return Err(syn::Error::new_spanned(value, "trait fn should be async"));
        }
        if value.sig.inputs.is_empty() {
            return Err(syn::Error::new_spanned(value, "trait fn no arg"));
        }
        let first_arg = value.sig.inputs.first().unwrap();
        if !matches!(first_arg, syn::FnArg::Receiver(_)) {
            return Err(syn::Error::new_spanned(
                first_arg,
                "trait fn first arg should be &self",
            ));
        }

        let attr = MethodAttrInfo::try_from(value.attrs.as_slice())?;
        let mut args = vec![];
        for arg in value.sig.inputs.iter_mut().skip(1) {
            match arg {
                syn::FnArg::Typed(pat) => {
                    let attr = ArgAttrInfo::try_from(&*pat)?;
                    args.push(attr);
                    pat.attrs.clear();
                }
                _ => {
                    unreachable!("trait fn arg have skip 1, others should be typed")
                }
            }
        }

        value.attrs.clear();
        Ok(Self {
            item: value,
            attr,
            args,
        })
    }
}

impl MethodInfo {
    fn generate_impl(&self) -> syn::ImplItemFn {
        let mut expr = String::new();
        expr.push_str("let uri = self.base_url.parse::<reqwest::Url>()?;");
        expr.push_str("let uri = uri.join(");

        let path_arg = self
            .args
            .iter()
            .filter_map(|input| match input {
                ArgAttrInfo::Path {
                    path_var_name,
                    arg_name,
                } => Some((path_var_name.clone(), arg_name.clone())),
                _ => None,
            })
            .collect::<Vec<(String, String)>>();

        // url path
        if path_arg.is_empty() {
            expr.push('"');
            expr.push_str(&self.attr.path);
            expr.push('"');
        } else {
            expr.push_str("format!(");
            expr.push('"');
            expr.push_str(&self.attr.path);
            expr.push('"');

            for (name, arg_name) in path_arg.iter() {
                expr.push_str(", ");
                expr.push_str(name.as_str());
                expr.push_str("=");
                expr.push_str(arg_name.as_str());
            }
            expr.push_str(").as_str()");
        }
        expr.push_str(")?;");
        expr.push_str("self.client.");
        expr.push_str(self.attr.method.as_str());
        expr.push_str("(uri)");

        // arg
        self.args.iter().for_each(|item| match item {
            ArgAttrInfo::Query {
                query_name,
                arg_name,
            } => {
                expr.push_str(".query(&[(");
                expr.push('"');
                expr.push_str(query_name.as_str());
                expr.push('"');
                expr.push_str(", ");
                expr.push_str(arg_name.as_str());
                expr.push_str(")])");
            }
            ArgAttrInfo::QueryMap { arg_name } => {
                expr.push_str(".query(");
                expr.push_str(arg_name.as_str());
                expr.push_str(")");
            }
            ArgAttrInfo::Json { arg_name } => {
                expr.push_str(".json(");
                expr.push_str(arg_name.as_str());
                expr.push_str(")");
            }
            ArgAttrInfo::Form { arg_name } => {
                expr.push_str(".form(");
                expr.push_str(arg_name.as_str());
                expr.push_str(")");
            }
            ArgAttrInfo::Header {
                header_name,
                arg_name,
            } => {
                expr.push_str(".header(");
                expr.push('"');
                expr.push_str(header_name.as_str());
                expr.push('"');
                expr.push_str(", ");
                expr.push_str(arg_name.as_str());
                expr.push_str(")");
            }
            ArgAttrInfo::HeaderMap { arg_name } => {
                expr.push_str(".headers(");
                expr.push_str(arg_name.as_str());
                expr.push_str(")");
            }
            _ => {}
        });
        // method header
        for (name, value) in self.attr.header.iter() {
            expr.push_str(".header(");
            expr.push('"');
            expr.push_str(name.as_str());
            expr.push('"');
            expr.push_str(", ");
            expr.push('"');
            expr.push_str(value.as_str());
            expr.push('"');
            expr.push_str(")");
        }

        expr.push_str(".send().await?");
        expr.push_str(".json().await.map_err(|e| e.into())");

        let expr = expr.parse::<TokenStream>().unwrap();
        let block = quote::quote!(
            pub fn mark (&self) {
                #expr
            }
        );
        let temp = syn::parse2::<syn::ImplItemFn>(block).unwrap();

        syn::ImplItemFn {
            attrs: vec![],
            vis: syn::Visibility::Inherited,
            defaultness: None,
            sig: self.item.sig.clone(),
            block: temp.block.clone(),
        }
    }

    fn generate_declare(&self) -> syn::TraitItemFn {
        self.item.clone()
    }
}

/// parse trait, get name, and feature
///
/// # Example
/// ```
/// #[async_fn_in_trait]
/// trait Client {
///    #[get("/user/{id}")]
///    async fn exmaple(&self, #[path("id")] id: u32) -> Result<(), Error>;
/// }
/// ```
///
/// ```
/// #[async_trait]
/// trait Client {
///    #[get("/user/{id}")]
///    async fn exmaple(&self, #[path("id")] id: u32) -> Result<(), Error>;
/// }
/// ```
///
/// ```
/// trait Client {
///    #[get("/user/{id}")]
///    async fn exmaple(&self, #[path("id")] id: u32) -> Result<(), Error>;
/// }
/// ```
///
/// because of [feature: async_fn_in_trait](https://rust-lang.github.io/rfcs/3185-static-async-fn-in-trait.html) not stable event in nightly,
/// so we need to know if user enable this feature.
///
///
/// `#[async_fn_in_trait]` and `#[async_trait]` make difference how to mark the trait define to generate.
///
/// no any attribute means not to generate trait, just impl.
///
#[derive(Debug)]
struct TraitAttrInfo {
    enable_async_trait: bool,
    enable_async_fn_in_trait: bool,
}

impl TryFrom<&syn::ItemTrait> for TraitAttrInfo {
    type Error = syn::Error;

    fn try_from(value: &syn::ItemTrait) -> Result<Self, Self::Error> {
        let mut enable_async_fn_in_trait = false;
        let mut enable_async_trait = false;

        for attr in value.attrs.iter() {
            if !matches!(attr.style, syn::AttrStyle::Outer) {
                continue;
            }
            match attr.meta {
                syn::Meta::Path(ref p) => {
                    if let Some(ident) = p.get_ident() {
                        match ident.to_string().as_str() {
                            "async_trait" => {
                                enable_async_trait = true;
                            }
                            "async_fn_in_trait" => {
                                enable_async_fn_in_trait = true;
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(Self {
            enable_async_fn_in_trait,
            enable_async_trait,
        })
    }
}

impl TraitAttrInfo {
     fn generate(&self, name: &str) -> TokenStream {
        let mut expr = String::new();

        expr.push_str("#[derive(Debug)]");
        expr.push_str("struct ");
        expr.push_str(name);
        expr.push_str("Builder {");
        expr.push_str("client: reqwest::Client,");
        expr.push_str("base_url: String,");
        expr.push_str("}");

        expr.push_str("#[derive(Debug)]");
        expr.push_str("struct ");
        expr.push_str(name);
        if self.enable_async_fn_in_trait || self.enable_async_trait {
            expr.push_str("Impl {");
        } else {
            expr.push_str(" {");
        }
        expr.push_str("client: reqwest::Client,");
        expr.push_str("base_url: String,");
        expr.push_str("}");

        expr.push_str("impl ");
        expr.push_str(name);
        expr.push_str("Builder {");
        expr.push_str("pub fn new() -> Self{");
        expr.push_str("Self{");
        expr.push_str("client: reqwest::Client::new(),");
        expr.push_str("base_url: \"http://localhost\".to_string(),");
        expr.push_str("}");
        expr.push_str("}");

        expr.push_str("pub fn base_url(self, s: String) -> Self{");
        expr.push_str("Self{");
        expr.push_str("base_url: s,");
        expr.push_str("..self");
        expr.push_str("}");
        expr.push_str("}");

        expr.push_str("pub fn client(self, client: reqwest::Client) -> Self{");
        expr.push_str("Self{");
        expr.push_str("client: client,");
        expr.push_str("..self");
        expr.push_str("}");
        expr.push_str("}");

        if self.enable_async_fn_in_trait || self.enable_async_trait {
            expr.push_str("pub fn build(self) -> impl ");
            expr.push_str(name);
            expr.push_str("{");
            expr.push_str(name);
            expr.push_str("Impl");
        } else {
            expr.push_str("pub fn build(self) -> ");
            expr.push_str(name);
            expr.push_str("{");
            expr.push_str(name);
        }
        expr.push_str("{");
        expr.push_str("client: self.client,");
        expr.push_str("base_url: self.base_url,");
        expr.push_str("}");
        expr.push_str("}");
        expr.push_str("}");

        proc_macro2::TokenStream::from_str(expr.as_str()).unwrap()
    }

     fn impl_name(&self, name: &str) -> String {
        if self.enable_async_fn_in_trait || self.enable_async_trait {
            format!("{}Impl", name)
        } else {
            name.to_string()
        }
    }
}

pub struct RetrofitTrait {
    trait_name: String,

    attr: TraitAttrInfo,
    method: Vec<MethodInfo>,
}

impl RetrofitTrait {
    fn generate_trait(&self) -> Option<syn::ItemTrait> {
        let mut expr = String::new();
        if !self.attr.enable_async_fn_in_trait && !self.attr.enable_async_trait {
            return None;
        }

        if self.attr.enable_async_trait {
            expr.push_str("#[async_trait]");
        }
        expr.push_str("trait ");
        expr.push_str(self.trait_name.as_str());
        expr.push_str("{");
        expr.push_str("}");

        let mut temp = syn::parse2::<syn::ItemTrait>(expr.parse::<TokenStream>().unwrap()).unwrap();
        self.method.iter().for_each(|method| {
            temp.items
                .push(syn::TraitItem::Fn(method.generate_declare()))
        });
        Some(temp)
    }

    fn generate_impl(&self) -> syn::ItemImpl {
        let mut expr = String::new();
        if self.attr.enable_async_fn_in_trait {
            expr.push_str("impl ");
            expr.push_str(&self.trait_name);
            expr.push_str(" for ");
            expr.push_str(self.attr.impl_name(&self.trait_name).as_str());
            expr.push_str("{");
        } else if self.attr.enable_async_trait {
            expr.push_str("#[async_trait]");
            expr.push_str("impl ");
            expr.push_str(&self.trait_name);
            expr.push_str(" for ");
            expr.push_str(self.attr.impl_name(&self.trait_name).as_str());
            expr.push_str("{");
        } else {
            expr.push_str("impl ");
            expr.push_str(self.attr.impl_name(&self.trait_name).as_str());
            expr.push_str("{");
        }
        expr.push_str("}");

        let mut temp = syn::parse2::<syn::ItemImpl>(expr.parse::<TokenStream>().unwrap()).unwrap();
        self.method
            .iter()
            .for_each(|method| temp.items.push(syn::ImplItem::Fn(method.generate_impl())));
        temp
    }

    pub fn generate(&self) -> TokenStream {
        let define = self.attr.generate(self.trait_name.as_str());
        let impl_st = self.generate_impl().into_token_stream();

        if let Some(trait_block) = self.generate_trait() {
            quote::quote!(
                #define
                #impl_st
                #trait_block
            )
        } else {
            quote::quote!(
                #define
                #impl_st
            )
        }
    }
}

impl syn::parse::Parse for RetrofitTrait {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let trait_item = input.parse::<syn::ItemTrait>()?;
        let attr = TraitAttrInfo::try_from(&trait_item)?;
        let mut method = vec![];
        for item in trait_item.items.into_iter() {
            match item {
                syn::TraitItem::Fn(func) => {
                    let info = MethodInfo::try_from(func)?;
                    method.push(info);
                }
                _ => {}
            }
        }
        Ok(Self {
            trait_name: trait_item.ident.to_string(),
            attr,
            method,
        })
    }
}

#[test]
fn test_impl(){
    let source = r#"trait Client {
        #[get("/users/{user}/repos")]
        async fn list_repos(&self, #[path("user")]  user: String) -> anyhow::Result<Vec<Repo>>;
    }"#;

    let token2 = proc_macro2::TokenStream::from_str(source).unwrap();
    let retrofit = syn::parse2::<RetrofitTrait>(token2).unwrap();

    println!("{}", retrofit.generate().to_string());
}