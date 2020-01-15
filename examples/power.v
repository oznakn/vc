int func p(base: int, pow: int)
    var p: int, i: int;

    p := 1;

    for i := 1 to pow by 1
        p := base * p;
    endfor;

    return p;
endfunc

int func main()
    print p(3, 2);
    print p(10, 2);
    print p(3, 4);
    print p(2, 8);
    print p(4, 3);

    return 0;
endfunc