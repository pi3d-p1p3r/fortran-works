#include <iostream>
using namespace std;

class Fraction{
    private:
    int a;
    int b;
    public:
    Fraction(int num, int denom):a(num),b(denom){}
    static Fraction multiply(const Fraction& frac1, const Fraction& frac2){
        int newNum = frac1.a*frac2.a;
        int newDenom = frac1.b*frac2.b;
        return Fraction(newNum,newDenom);
    }
    void display() const{
        cout << a << "/" << b << endl;
    }
};

int main(){
    Fraction fraction1(1,2);
    Fraction fraction2(3,4);
    Fraction result = Fraction::multiply(fraction1,fraction2);
    result.display();
    return 0;
}