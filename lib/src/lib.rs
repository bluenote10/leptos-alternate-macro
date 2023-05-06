mod macro_c;
mod macro_fragment;
mod utils;

#[cfg(test)]
mod test_utils;

use proc_macro::TokenStream;

#[proc_macro]
pub fn c(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let output = macro_c::c_impl(input);

    proc_macro::TokenStream::from(output)
}

#[proc_macro]
pub fn fragment(input: TokenStream) -> TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let output = macro_fragment::fragment_impl(input);

    proc_macro::TokenStream::from(output)
}
