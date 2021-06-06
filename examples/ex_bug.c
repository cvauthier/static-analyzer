int x = rand(-100, 100);

int bar(int x)
{
	return x*x;
}

int foo(int x, int y)
{
	return bar(x)+bar(y);
}

void main()
{
	int i = foo(0,1);
	i += bar(x);
}
