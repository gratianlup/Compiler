int prime(int n) {
	if(n <= 2) return 1;
	if(n % 2 == 0) return 0;
	
	for(int i = 3; i < n/2; i += 2) {
		if(n % i == 0) return 0;
	}
	
	return 1;
}

int fibo(int n) {
	if(n <= 1) return 1;
	else return fibo(n - 1) + fibo(n - 2);
}

int palindrom(int n) {
	int d[9];
	int ct = 0;
	while(n) {
		d[ct++] = n % 10;
		n /= 10;
	}
	
	for(int i = 0; i < ct; i++) {
		if(d[i] != d[9 - i - 1]) return 0;
	}
	
	return 1;
}
