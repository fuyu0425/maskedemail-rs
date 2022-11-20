extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Item, ItemFn, Stmt};

#[proc_macro_attribute]
pub fn with_client(args: TokenStream, stream: TokenStream) -> TokenStream {
    let input = parse_macro_input!(stream as ItemFn);
    let ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = input;
    let stmts = &block.stmts;
    let ret = quote! {
        #(#attrs)* #vis #sig {
            let client = get_client();
            #(#stmts)*
        }
    };
    ret.into()
}
