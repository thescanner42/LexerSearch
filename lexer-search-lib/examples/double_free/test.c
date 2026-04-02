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
