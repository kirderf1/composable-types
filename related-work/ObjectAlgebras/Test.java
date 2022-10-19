class Test {
    
    <T> T threePlusFive(ExprAlg<T> alg) {
        return alg.add(alg.cons(3), alg.cons(5));
    }
    
//     <T> T twoMulThreePlusFive(ExprAlg<T> alg) {
//         return alg.mul(alg.cons(2), threePlusFive(new ExprEval()));
//     }

    
    
    public static void main(String args[]) {
        Test t = new Test();
        t.print();
    }
 
    void print() {
        System.out.println("Evaluation example:");
        System.out.println(threePlusFive(new ExprEval()).eval());
    }
 
}
