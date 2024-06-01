public class Example2{
static void funcion(){
boolean var_3 = ((false!=true)&&(!((true==false)||false)));
int var_0 = ((1+3)*(2/(4-5)));
boolean var_4 = (!true);
boolean var_2 = (!var_4);
if (var_4){
var_0 = (var_0+1);
} else {
var_0 = (var_0-1);
};
int var_1 = 10;
while ((var_1>0)){
var_1 = (var_1-1);
};
}
static int metodo(int var_5, int var_6){
while ((var_5!=0)){
if ((var_5<var_6)){
var_6 = (var_6-var_5);
} else {
var_5 = (var_5-var_6);
};
};
return 0;
}
static void sintaxisExtra(){
int var_8, var_9, var_7;
var_8 = 0;
var_9 = 1;
var_7 = 2;
var_8 = (var_8+5);
var_8 = (var_8+1);
var_8 = (var_8-1);
var_9 = ((true==(var_8>1)) ? var_8 : 0);
}
public static void main(String[] args) {
funcion();
sintaxisExtra();
int var_10 = metodo(1, 2);
}
}