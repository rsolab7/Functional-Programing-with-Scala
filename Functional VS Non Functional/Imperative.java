public class Imperative {

    //Fizz Buzz
    //  https://en.wikipedia.org/wiki/Fizz_buzz
    public static void fizzBuzz(int n) {
        for (int i = 1; i <= n; i++) {
            if (i % 5 == 0 && i % 3 == 0) {
                System.out.println("FizzBuzz");
            } else if (i % 5 == 0) {
                System.out.println("Buzz");
            } else if (i % 3 == 0) {
                System.out.println("Fizz");
            } else {
                System.out.println(i + ""); //coarse int to String
            }
        }
    }

    //Factorial
    public static long fact(int x) {
        long result = 1;
        for (int i = 1; i <= x; i++) {
            result *= i;
        }
        return result;
    }


    public static void main(String args[]) {
        Imperative.fizzBuzz(20);

        System.out.println("fact(0) == " + Imperative.fact(0));
        System.out.println("fact(5) == " + Imperative.fact(5));
        System.out.println("fact(11) == " + Imperative.fact(11));
    }

}
