int *var[5]; /* Array of pointers to int values */
int (*var2)[5]; /* Pointer to array of int values */
long *var3( long, long ); /* Function returning pointer to long */
long (*var4)( long, long ); /* Pointer to function returning long */

struct both       /* Array of pointers to functions */
{                 /*   returning structures         */
    int a;
    char b;
} ( *var5[5] )( struct both, struct both );

unsigned int *(* const *var6[2][2] ) ( void );

double ( *var7( double (*)[3] ) )[3];

int (*var8[3])(int *x, int *y);

int (*var9(int (*)(long), int))(int, ...);
