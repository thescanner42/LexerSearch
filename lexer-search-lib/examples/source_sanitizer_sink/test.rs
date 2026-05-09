{
    let x = source;
    let x1 = x;
    sanitize(&mut x1);
    let x2 = x1;
    sink(x2);
}


{
    let x = source;
    let x1 = x;
    let x2 = x1;
    sink(x2);
}


