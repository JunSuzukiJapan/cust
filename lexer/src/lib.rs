mod tokenizer;
mod tokenizer_error;
mod location;

use tokenizer_error::TokenizerError;
use location::Location;



#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
