void f() {
	int a;
	switch(a) {
		case 1: return;
		case 2: {
			a = a + 1;
			break;
		}
		case 3: break;
		case 4:
		case 5:
		case 6: {
			switch(a * 2) {
				int a;
				default: break;
			}
		}
		default: return;
	}
	
	return;
}