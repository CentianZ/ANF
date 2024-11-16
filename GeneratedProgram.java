import java.util.*;
public class GeneratedProgram {

    static class Constructor {
        String name;
        List<Object> args;

        Constructor(String name, List<Object> args) {
            this.name = name;
            this.args = args;
        }

        String getTag() { return name; }
        Object getArg(int index) { return args.get(index); }
    }

    public static Object my_double(Object val) {
        Object res = null;
        Object t0 = null;
        t0 = val;
        Object t1 = null;
        t1 = Integer.valueOf(2);
        res = (Integer) t0 * (Integer) t1;


        return res;
    }

    public static Object my_factorial(Object x) {
        Object res = null;
        Object t2 = null;
        Object t0 = null;
        t0 = x;
        Object t1 = null;
        t1 = Integer.valueOf(0);
        t2 = (Integer) t0 == (Integer) t1;


        if ((boolean) t2) {
        res = Integer.valueOf(1);
} else {
        Object t3 = null;
        t3 = x;
        Object t7 = null;
        Object t6 = null;
        Object t4 = null;
        t4 = x;
        Object t5 = null;
        t5 = Integer.valueOf(1);
        t6 = (Integer) t4 - (Integer) t5;


        t7 = my_factorial(t6);

        res = (Integer) t3 * (Integer) t7;


        }


        return res;
    }

    public static Object my_fst(Object p) {
        Object res = null;
        Object t0 = null;
        t0 = p;
        Object scrutVar = null;
        scrutVar = t0;
        switch(((Constructor) scrutVar).getTag()) {
            case "MkPair":
                Object x = ((Constructor) scrutVar).getArg(0);
                Object y = ((Constructor) scrutVar).getArg(1);

        res = x;
                break;

        }


        return res;
    }

    public static Object my_snd(Object p) {
        Object res = null;
        Object t0 = null;
        t0 = p;
        Object scrutVar = null;
        scrutVar = t0;
        switch(((Constructor) scrutVar).getTag()) {
            case "MkPair":
                Object x = ((Constructor) scrutVar).getArg(0);
                Object y = ((Constructor) scrutVar).getArg(1);

        res = y;
                break;

        }


        return res;
    }

    public static Object my_sum(Object xs) {
        Object res = null;
        Object t0 = null;
        t0 = xs;
        Object scrutVar = null;
        scrutVar = t0;
        switch(((Constructor) scrutVar).getTag()) {
            case "Nil":

        res = Integer.valueOf(0);
                break;
            case "Cons":
                Object y = ((Constructor) scrutVar).getArg(0);
                Object ys = ((Constructor) scrutVar).getArg(1);

        Object t3 = null;
        t3 = y;
        Object t5 = null;
        Object t4 = null;
        t4 = ys;
        t5 = my_sum(t4);

        res = (Integer) t3 + (Integer) t5;


                break;

        }


        return res;
    }

    public static Object my_testlist() {
        Object res = null;
        Object t0 = null;
        t0 = Integer.valueOf(1);
        Object t3 = null;
        Object t1 = null;
        t1 = Integer.valueOf(2);
        Object t2 = null;
        t2 = new Constructor("Nil", Arrays.asList());
        t3 = new Constructor("Cons", Arrays.asList(t1, t2));


        res = new Constructor("Cons", Arrays.asList(t0, t3));


        return res;
    }

    public static Object my_map(Object f, Object xs) {
        Object res = null;
        Object t0 = null;
        t0 = xs;
        Object scrutVar = null;
        scrutVar = t0;
        switch(((Constructor) scrutVar).getTag()) {
            case "Nil":

        res = new Constructor("Nil", Arrays.asList());
                break;
            case "Cons":
                Object y = ((Constructor) scrutVar).getArg(0);
                Object ys = ((Constructor) scrutVar).getArg(1);

        Object t4 = null;
        Object t3 = null;
        t3 = y;
        t4 = my_f(t3);

        Object t7 = null;
        Object t5 = null;
        t5 = f;
        Object t6 = null;
        t6 = ys;
        t7 = my_map(t5, t6);


        res = new Constructor("Cons", Arrays.asList(t4, t7));


                break;

        }


        return res;
    }

    public static void main(String[] args) {
        Object result;
        Object t2 = null;
        Object t0 = null;
        t0 = Integer.valueOf(1);
        Object t1 = null;
        t1 = Integer.valueOf(2);
        t2 = new Constructor("MkPair", Arrays.asList(t0, t1));


        result = my_fst(t2);

        System.out.println(result);
    }

}