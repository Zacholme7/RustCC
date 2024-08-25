/* Test that we recognize XMM0 is not live at exit
 * when we return a struct only in general-purpose registers;
 * similar to fourteen_pseudos_interfere.c but returning a struct
 * instead of an int. Test script validates that the only memory access
 * instructions are populating and transferring the return value struct.
 * There should be at most ten memory access instructions: for each
 * of this function's two return statements (success and failure), three
 * instructions populate the return value struct and two move it into registers.
 * tests/chapter_20/helper_libs/return_all_instr_struct_lib.c defines a
 * 'target' function that calls 'return_struct' and validates the result.
 *
 * This test program is generated from templates/chapter_20_templates/fourteen_pseudos_interfere.c.jinja.
 * */
struct s {
    int a;
    int b;
    long l;
};

double glob = 20.0;
double glob2 = 30.0;
int glob3 = 40.0;

struct s return_struct(void) {
    // Create a clique of 14 tmps that interfere;
    // we can color all of them w/out spilling anything.
    double a = glob * glob;
    double b = glob2 + 2.0;
    double c = a + 5.0;
    double d = b - glob3;
    double e = glob + 7.0;
    double f = glob2 * 2.0;
    double g = c * 3.0;
    double h = d * 112.;
    double i = e / 3.0;
    double j = g + f;
    double k = h - j;
    double l = i + 1000.;
    double m = j - d;
    double n = m * l;

    if (a == 400.0 && b == 32.0 && c == 405.0 && d == -8.0 && e == 27.0 &&
        f == 60.0 && g == 1215.0 && h == -896. && i == 9.0 && j == 1275. &&
        k == -2171. && l == 1009. && m == 1283. && n == 1294547.) {
        struct s retval = {20, 30, 40};
        return retval; // success
    } else {
        struct s retval = {-1, -2, -3};
        return retval; // fail
    }

}