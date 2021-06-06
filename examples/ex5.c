
int foo3(int z3)
{
	return z3*z3;
}

int foo2(int z2)
{
	return foo3(z2)-foo3(z2+1);
}

int foo1(int z1)
{
	return foo2(z1)+foo2(z1+1);
}

void main()
{
	int i = foo1(10);
}
