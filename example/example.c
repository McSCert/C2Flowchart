
typedef struct
{
		int a;
		int b;
		int c;
		int d;
} sInputs;


int someCode(sInputs inputs, unsigned char k)
{
	int x = 0;
	int y = inputs.a * inputs.b + inputs.c - inputs.d;
	if (inputs.a < inputs.b)
	{
		if (k)
		{
			x = 1; 
		} 
		else
		{
			x = 2;  
		}    
	}
	else if (inputs.c < inputs.b)
	{
		if (k)
		{
			x = 3; 
		} else
		{
			x = 4;  
		}
	}
	else
	{
		if (k)
		{
			x = 5; 
		} else
		{
			x = 6;  
		}
	}
	return x/y;
}
