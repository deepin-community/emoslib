#include <stdio.h>

int main()
{
	int in    = 0;
	char r    = 0;
	int count = 0;
	while((in = getchar()) != EOF)
	{
		if(in != '\n')
		{
			if(in == '1')
				r |= (1 << (7-count));
			count++;
		}
		if(count==8)
		{
			printf("%c",r);
			r = 0;
			count=0;
		}
	}
}
