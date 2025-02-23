#include <iostream>
using namespace std;

class MyFraction{
private:
    int a,b,c,d;
public:
    double MyFraction(int a, int b, int c, int d){
        return (a*d+b*c)/(b*d);
    }
    double frac(int a, int b, int c, int d){
        return (a*d+b*c)/(b*d);
    }
};
int main(){
    MyFraction frac;
    return 0;
}
