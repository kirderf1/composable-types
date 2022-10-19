class Test {
    
    // Examples of expressions
    <T> T threePlusFive(ExprAlg<T> alg) {
        return alg.add(alg.cons(3), alg.cons(5));
    }
    
    <T> T twoMulThreePlusFive(ExprAlg<T> alg) {
        return alg.mul(alg.cons(2), threePlusFive(alg));
    }

    <T> T threePlusNegFive(NegAlg<T> alg) {
        return alg.add(alg.cons(3), alg.neg(alg.cons(5)));
    }

    // Main, printing results of examples
    public static void main(String args[]) {
        Test t = new Test();
        t.print();
    }
 
    // Helper printer function
    void print() {
        System.out.println("Evaluation examples:");
        System.out.println(twoMulThreePlusFive(new ExprEval()).eval());
        System.out.println(threePlusNegFive(new NegEval()).eval()); 
        System.out.println("AsString example:");
        System.out.println(threePlusNegFive(new NegAsString()).asString()); 
    }
}
