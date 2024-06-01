public class Example{
public static void main(String[] args) {
int[] var_0 = {10, 9, 8};
sort(var_0);
}
static void sort(int[] var_4){
int var_2 = 0;
int var_1 = var_4.length;
while ((var_2<var_1)){
int var_5 = var_2;
while ((var_5>0)){
if ((var_4[(var_5-1)]>var_4[var_5])){
int var_3 = var_4[var_5];
var_4[var_5] = var_4[(var_5-1)];
var_4[(var_5-1)] = var_3;
};
var_5 = (var_5-1);
};
var_2 = (var_2+1);
};
}
}