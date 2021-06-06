int a = rand(-100, 100);

int bar(int x)
{
	return x*x*x;
}

int foo(int x, int y)
{
	return bar(x)/bar(y);
}

void main()
{
	int i = rand(-20,20);
	
	i = foo(2*i,i);
	i += bar(a);
}

