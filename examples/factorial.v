int func f(x: int)
    if x = 1 then
        return 1;
    endif;

    return x * f(x - 1);
endfunc

int func main()
    print f(3);
    print f(4);
    print f(10);

    return 0;
endfunc