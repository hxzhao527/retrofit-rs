
mod ttt;

#[proc_macro]
pub fn retrofit(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let token2 = proc_macro2::TokenStream::from(item);
    let retrofit = syn::parse2::<ttt::RetrofitTrait>(token2).unwrap();

    retrofit.generate().into()
}