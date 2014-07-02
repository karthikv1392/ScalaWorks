import java.util.Scanner;


public class fibo_rec {

	/**
	 * @param args
	 */
	public  static int fibo (int num)
	{
		int[] lookup=new int[100];
		int index;
		for(index=0;index<100;index++)
		{
			lookup[index]=0;			
		}
		if(lookup[num]== 0)
		{
			if(num<=1)
				lookup[num]=num;
			else
				lookup[num]=fibo(num-1) + fibo(num-2);
		}
		return lookup[num];
	}
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		Scanner in=new Scanner(System.in);
		System.out.println("Enter the Fibonacci number to be found");
		int number=in.nextInt();
		System.out.println(fibo(number));
	}

}
