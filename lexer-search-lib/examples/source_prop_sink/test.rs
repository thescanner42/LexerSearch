let x = source;
let x1 = x;

let x2 = throwaway;
let x3 = x2;

// same as let x2 = x1;
let _ = std::mem::replace(&mut x2, x1);

// same as let x3 = x2;
let _ = rev_replace(x2, &mut x3);

sink(x3);
