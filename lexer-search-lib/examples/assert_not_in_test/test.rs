fn main() {
    assert_eq!(1, 1); // assert out of test
    assert_eq!(1, 1); // assert out of test
}

#[cfg(test)]
mod tests {
    #[test]
    fn this_is_a_test() {
        assert_eq!(1, 1);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    #[should_panic]
    fn this_is_a_test() {
        assert_eq!(1, 1);
    }
}
