
void main()
{
	int x = rand(0,10);
	while(1 > 0)
	{
		if (x < 10000)
		{
			x++;
		}
		else
		{
			x *= -1;
		}
		assert(x <= 10000);
		assert(x >= -10000);
	}
}
