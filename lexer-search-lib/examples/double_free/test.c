{
    int* abc = malloc(sizeof(int));
    {
        int* cde = malloc(sizeof(int)); // overlap
        free(abc);
    }
    free(abc); // bad
}

{
    int* abc = malloc(sizeof(int));
    free(abc);
    abc = NULL;
    free(abc);
}

{
    int* abc = malloc(sizeof(int));
    free(abc);
    abc = malloc(sizeof(int));
    free(abc);
}

void tester(int* abc) {
    free(abc);
    free(abc); // also bad
}

void tester(my_type* abc) {
    free(abc->other);
    free(abc->other); // also bad
}

void tester(my_type* abc) {
    free(abc[0].nested->field);
    free(abc[0].nested->field); // also bad
}
