public class Example2{
static void funcion(){
boolean var_85 = ((false!=true)&&(!((true==false)||false)));
int var_82 = ((1+3)*(2/(4-5)));
boolean var_86 = (!true);
boolean var_84 = (!var_86);
if (var_86){
var_82 = (var_82+1);
} else {
var_82 = (var_82-1);
};
int var_83 = 10;
while ((var_83>0)){
var_83 = (var_83-1);
};
}
static int metodo(int var_87, int var_88){
while ((var_87!=0)){
if ((var_87<var_88)){
var_88 = (var_88-var_87);
} else {
var_87 = (var_87-var_88);
};
};
return 0;
}
static void sintaxisExtra(){
int var_90, var_91, var_89;
var_90 = 0;
var_91 = 1;
var_89 = 2;
var_90 = (var_90+5);
var_90 = (var_90+1);
var_90 = (var_90-1);
var_91 = ((true==(var_90>1)) ? var_90 : 0);
}
public static void main(String[] args) {
funcion();
sintaxisExtra();
int var_92 = metodo(1, 2);
}
}