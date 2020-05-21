#include <sstream>
#include <iostream>
#include <limits>
#include <stack>

std::ostream& operator<< (std::ostream& os, __int128 x)
{
    std::ostringstream ost;
    if (x < 0) { ost << '-'; x = -x; }
    std::stack<char> s;
    while (x >= 10) { s.push(x % 10); x /= 10; }
    if (x % 10) s.push(x % 10);
    while (not s.empty()) { ost << static_cast<char>('0' + s.top()); s.pop(); }
    os << ost.str();
    return os;
}

template<const unsigned P, typename int_t = int64_t>
class Fixed
{
public:
    explicit Fixed(const int_t v) : m_value(v)
    {
        if (not rshift(P))
        {
            m_value = getNaN();
        }
    }

    bool isNaN() const { return getNaN() == m_value; }

    Fixed& operator/= (const Fixed& f);

    template<const unsigned PP, typename IT>
    friend std::ostream& operator<< (std::ostream&, const Fixed<PP, IT>&);

private:
    bool rshift(unsigned p)
    {
        while (p--) if (__builtin_mul_overflow(m_value, 10, &m_value)) return false;
        return true;
    }
    static int_t getNaN()
    {
        return std::numeric_limits<int_t>::min();
    }

    int_t m_value;  ///< fixed point value with P decimal digits after decimal point
};

template<const unsigned P, typename int_t>
Fixed<P,int_t>& Fixed<P,int_t>::operator/= (const Fixed<P,int_t>& f)
{
    std::clog << "Fixed<" << P << ">::operator /=, dividing " << *this << " by " << f << std::endl;

    if (0 == m_value)
    {
        return *this;
    }

    if (0 == f.m_value or isNaN() or f.isNaN())
    {
        m_value = getNaN();
        return *this;
    }

    // N / D = (Q, R), N = Q * D + R
    const bool isNegative = (m_value < 0) ^ (f.m_value < 0);
    std::clog << "Fraction is " << (isNegative ? "negative" : "positive") << std::endl;

    // Shift numerator to the left as much as possible
    int_t N = m_value < 0 ? -m_value : m_value;
    int   npow = P;
    int_t tmp;
    
    while (not __builtin_mul_overflow(N, 10, &tmp))
    {
        N = tmp;
        ++npow;
    }

    // Shift denominator to the right as much as possible
    int_t D = f.m_value < 0 ? -f.m_value : f.m_value;
    int   dpow = P;
    while (0 == D % 10)
    {
        D /= 10;
        --dpow;
    }

    // Initialize Q
    int_t Q = N / D;

    std::clog << "N = " << N << "*10^{" << npow * -1 << "}\tD = " << D << "*10^{" << dpow * -1 << "}\tQ = " << Q << "*10^{" << dpow - npow << '}' << std::endl;

    // Initialize fraction as 0 and begin it's accumulation.
    m_value = 0;

    // We need at least P decimal digits. Thus we need Q in a form "integer * 10^{-(P+1)}" (need one more digit so we can ensure the last digit is precise).
    // Now it is represented as "integer * 10^{-(npow - dpow)}". Thus (npow - dpow) should be > P to be at least P+1.
    do
    {
        if (0 == N)
        {
            break;
        }

        if (npow - dpow > static_cast<int>(P))
        {
            std::clog << "No precision loss, partial Q = " << Q << "*10^{" << dpow - npow << '}' << std::endl;

            // Got there, shift Q right so it has P precise decimal digits after the decimal point.
            int_t q = Q;
            int   qpow = npow - dpow;
            while (qpow > static_cast<int>(P) + 1)
            {
                q /= 10;
                qpow -= 1;
            }
            // Now qpow = P + 1, get the last digit.
            if (q % 10 >= 5)
            {
                q /= 10;
                ++q;
            }
            else
            {
                q /= 10;
            }
            std::clog << "\taccumulating " << q << "*10^{-" << P << '}' << std::endl;
            m_value += q;

            break;
        }
        else
        {
            std::clog << "Precision loss, partial Q = " << Q << "*10^{" << dpow - npow << '}' << std::endl;

            // Accumulate m_value with Q*10^{dpow - npow}
            int_t q = Q;
            int   qpow = npow - dpow;
            while (qpow < static_cast<int>(P))
            {
                int_t tmp;
                if (__builtin_mul_overflow(q, 10, &tmp))
                {
                    m_value = getNaN();
                    return *this;
                }
                q = tmp;
                ++qpow;
            }
            std::clog << "\tacumulating " << q << "*10^{-" << P << '}' << std::endl;
            m_value += q;

            // Repeat with the residual.
            N -= Q * D;
            if (0 == N)
            {
                break;
            }
            while (not __builtin_mul_overflow(N, 10, &tmp))
            {
                N = tmp;
                ++npow;
            }
            Q = N / D;
            std::clog << "N = " << N << "*10^{" << npow * -1 << "}\tD = " << D << "*10^{" << dpow * -1 << "}\tQ = " << Q << "*10^{" << dpow - npow << '}' << std::endl;
        }
    } while (true);

    if (isNegative)
    {
        m_value = -m_value;
    }

    return *this;
}

template<const unsigned P, typename IT>
std::ostream& operator<< (std::ostream& os, const Fixed<P, IT>& f)
{
    if (Fixed<P, IT>::getNaN() == f.m_value)
    {
        os << "NaN";
        return os;
    }

    IT decimalpt = 1;
    unsigned u = P;
    while (u--)
    {
        decimalpt *= 10;
    }

    std::ostringstream ost;
    if (f.m_value >= 0)
    {
        ost << std::dec << f.m_value / decimalpt << '.';
        ost.width(P);
        ost.fill('0');
        ost << f.m_value % decimalpt;
    }
    else
    {
        ost << '-' << std::dec << -f.m_value / decimalpt << '.';
        ost.width(P);
        ost.fill('0');
        ost << -f.m_value % decimalpt;
    }

    os << ost.str();

    return os;
}

int main()
{
{
    Fixed<3, __int128> foo(300);
    Fixed<3, __int128> bar(0);

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<__int128*>(&bar) = -5555;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<__int128*>(&bar) = 90;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<__int128*>(&bar) = -1;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;
}
{
    Fixed<2> foo(300);
    Fixed<2> bar(0);

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<int64_t*>(&bar) = -5555;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<int64_t*>(&bar) = 90;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<int64_t*>(&bar) = -1;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;
}
{
    Fixed<3> foo(300);
    Fixed<3> bar(0);

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<int64_t*>(&bar) = -5555;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<int64_t*>(&bar) = 90;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<int64_t*>(&bar) = -1;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;
}
{
    Fixed<2> foo(300);
    Fixed<2> bar(0);

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<int64_t*>(&bar) = -5555;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<int64_t*>(&bar) = 90;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<int64_t*>(&bar) = -1;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;
}
{
    Fixed<3, short> foo(30);
    Fixed<3, short> bar(0);

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<short*>(&bar) = -5555;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<short*>(&bar) = 90;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<short*>(&bar) = -1;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;
}
{
    Fixed<2, short> foo(300);
    Fixed<2, short> bar(0);

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<short*>(&bar) = -5555;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<short*>(&bar) = 90;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

    std::cout << "====================================================" << std::endl;
    *reinterpret_cast<short*>(&bar) = -1;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;
}
    Fixed<4, long> foo(1);
    Fixed<4, long> bar(0);
    std::cout << "====================================================" << std::endl;
    std::cout << foo << '/' << bar << std::endl;
    foo /= bar;
    std::cout << foo << std::endl;

#define tc(u) do {\
        Fixed<u, __int128> foo(498765);\
        Fixed<u, __int128> bar(999999);\
        std::cout << "====================================================" << std::endl;\
        std::cout << foo << '/' << bar << std::endl;\
        foo /= bar;\
        std::cout << foo << std::endl;\
} while(false);
    tc(0);
    tc(1);
    tc(2);
    tc(3);
    tc(4);
    tc(5);
    tc(6);
    tc(7);
    tc(8);
    tc(9);
    tc(10);
    tc(11);
    tc(12);
    tc(13);
    tc(14);
    tc(15);
}
