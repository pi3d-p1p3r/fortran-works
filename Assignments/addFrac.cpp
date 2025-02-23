#include <iostream>

class Fraction {
private:
    int a;
    int b;

public:
    Fraction(int num, int denom) : a(num), b(denom) {}

    static Fraction add(const Fraction& frac1, const Fraction& frac2) {
        int newNumerator = frac1.a * frac2.b + frac2.a * frac1.b;
        int newDenominator = frac1.b * frac2.b;
        return Fraction(newNumerator, newDenominator);
    }

    void display() const {
        std::cout << a << "/" << b << std::endl;
    }
};

int main() {
    Fraction fraction1(1, 2);
    Fraction fraction2(3, 4);

    Fraction result = Fraction::add(fraction1, fraction2);

    std::cout << "Fraction 1: ";
    fraction1.display();

    std::cout << "Fraction 2: ";
    fraction2.display();

    std::cout << "Sum: ";
    result.display();

    return 0;
}
