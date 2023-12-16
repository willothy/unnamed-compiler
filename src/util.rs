use chumsky::{primitive::just, text::whitespace, IterParser, Parser};

pub fn comma_separated<'a, P: 'a + crate::Parser<'a, T>, T: 'a>(
    parser: P,
) -> impl crate::Parser<'a, Vec<T>> {
    parser
        .separated_by(just(",").then_ignore(whitespace()))
        .collect::<Vec<_>>()
        .boxed()
}
