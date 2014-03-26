package task02;

// Этот класс должен быть реализован на Clojure
import task02.Database;

class Client 
{
		public static void main(String[] args) 
		{
			if(args.length == 0) {
				System.out.println("Usage: Client query...");
				System.exit(0);
			}
			Database.InitDatabase();
			try {
				String res = Database.Select(args[0]);
				System.out.println("Result of query '"+args[0]+"' is:");
				System.out.println(res);
			}
			catch (Throwable e) {
				System.out.println("Error " + e.getMessage());
				e.printStackTrace();
			}

		}
}

