real func p(base: int, pow: int)
    var p: real, i: int;

    p := 1.0;

    for i := 1 to pow by 1
        p := base * p;
        print p;
    endfor;

    return p;
endfunc

int func main()
    print p(3, 2);

    return 0;
endfunc
