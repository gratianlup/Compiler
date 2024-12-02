void memcpy(void* dst, void* src, int count) {
	while (count--) {
		*(char *)dst = *(char *)src;
		dst = (char *)dst + 1;
		src = (char *)src + 1;
	}
}

int strcmp (const char * src, const char * dst) {
	int ret = 0 ;

	while( ! (ret = *(unsigned char *)src - *(unsigned char *)dst) && *dst)
			++src, ++dst;
	
	if ( ret < 0 )
			ret = -1 ;
	else if ( ret > 0 )
			ret = 1 ;

	return( ret );
	return 0;
}

char* strrchr (const char * string, int ch){
        char *start = (char *)string;

        while (*string++)                  
                ;
                                             
        while (--string != start && *string != (char)ch)
                ;

        if (*string == (char)ch)         
                return( (char *)string );

        return(0);
}