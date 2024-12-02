typedef int (*SortFunct)(void* a, void* b);
typedef void (*SwapFunct)(void* a, void* b);

int IntCompare(void*a, void *b) {
	int* ia = (int*)a;
	int* ib = (int*)b;
	if(*ia < *ib) return -1;
	else if(*ia > *ib) return 1;
	else return 0;
}

void IntSwap(void* a, void* b) {
	int temp = *(int*)a;
	*(int*)a = *(int*)b;
	*(int*)b = temp;
}

void Sort(void* base, int n, int size, 
		  SortFunct comp, SwapFunct swap) {
	for(int i = 0; i < n; i++) {
		for(int j = i + 1; j < n; j++) {
			void* pi = (char*)base + i*size;
			void* pj = (char*)base + j*size;
			if(comp(pi, pj) > 0) {
				swap(pi, pj);
			}
		}
	}
}

int a[] = {3,4,5,1,2,8,6};

int main() {
	Sort(a, sizeof(a) / sizeof(int), sizeof(int),
		 IntCompare, IntSwap);
	return 0;
}
