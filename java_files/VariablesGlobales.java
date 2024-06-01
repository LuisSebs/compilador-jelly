public class VariablesGlobales{
static int global1 = 0;
static boolean global2 = true;
static int g3, g4, g5;
static int lon = 90;
static int i = 2;
static boolean truent = false;
static int[] calificaciones;
static void iterador(int var_0, int var_1){
while (global2){
while ((global1<var_0)){
global1 = (global1+var_1);
};
if ((global1>=var_0)){
global2 = false;
};
};
}
public static void main(String[] args) {
iterador(10, 1);
}
}