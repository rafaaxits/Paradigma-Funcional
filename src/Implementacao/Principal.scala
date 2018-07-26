package Implementacao
import java.io.File
import java.io._
import scala.io.Source
import sun.security.util.Length
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

object Principal {

 
  def main(args: Array[String]): Unit = {
    leitura(List(0, 0, 0, 0));
    //println(tamanhoLista(List(9,23,3,56)))
    //println(tamanhoLista(List()))
  }
  
    def leitura(contadores: List[Int]) {
     val inicio = System.currentTimeMillis();
     val input = new BufferedInputStream(new FileInputStream("src/Arquivos/poker2M.txt")); 
     val br = new BufferedReader(new InputStreamReader(input));
     escrita(preProcessamento(br, contadores), inicio);
     br.close()
     input.close()
    }
    
    
   def preProcessamento(br : BufferedReader, contadores: List[Int]) : List[Int] = {  
     if(br.ready()){
       preProcessamento(br, processamento(conversion(br.readLine().split(" ").toList).collect{ case (a: Int) => (a)}.
          sortBy(x =>x match {case i: Int => i}), contadores) )
     }else{
       contadores
     }
   }
   
   def processamento(mao: List[Int], contadores: List[Int]) : List[Int] ={
     if(quatroIguais(mao, 0) == true){
       incrementaAlgumContador(0, contadores)
       }else{
         if(sequencia(mao) == true){
           incrementaAlgumContador(1, contadores)
         }else{
           if(todosDiferentes(mao) == true){
             incrementaAlgumContador(2, contadores)
           }else{
             incrementaAlgumContador(3, contadores)
           }
         }
       }
   }
   
   def incrementaAlgumContador(index: Int, contadores: List[Int]) : List[Int] = {
     if(index == 0){
       contadores.head + 1 :: contadores.tail
     }else {
       contadores.head :: incrementaAlgumContador(index - 1, contadores.tail)
     }
   }
   
   def escrita(contadores: List[Int], inicio: Long) = {
    val file = new File("src/Arquivos/saida.txt")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(Math.abs(inicio - System.currentTimeMillis()) + " | " + contadores.head + " | " + valorDoIndex(2, contadores) + " | " + valorDoIndex(1, contadores))
    bw.close()
   }
   
   def conversion (line: List[Any]) ={
         line.map(e => 
         if(e=="T")10
         else if(e=="J")11
         else if(e=="Q")12
         else if(e=="K")13
         else if(e=="A")14
         else if(e=="2")2
         else if(e=="3")3
         else if(e=="4")4
         else if(e=="5")5
         else if(e=="6")6
         else if(e=="7")7
         else if(e=="8")8
         else if(e=="9")9)
     }
  
  def valorDoIndex(index: Int, mao: List[Int]): Int ={
    if(index == 0){
      mao.head
    }else{
      valorDoIndex(index-1, mao.tail)
    }
  }
  
  def quatroIguais(mao: List[Int], count4Iguais: Int): Boolean ={
    if(mao.head == valorDoIndex(3, mao) || (valorDoIndex(1, mao) == valorDoIndex(4, mao))){
      true
    }else{
      false
    }
   }
  
  def sequencia(mao: List[Int]): Boolean ={
    if(mao.length < 2){
      true
    }else{
      if(mao.head != valorDoIndex(1, mao)-1){
      false
    }else{
      sequencia(mao.tail)
    }
  }
 }
  
  def todosDiferentes(mao: List[Int]): Boolean ={
     if(mao.length<2){
       true
     }else{
       if(mao.head == valorDoIndex(1,mao)){
         false
       }else {
         todosDiferentes(mao.tail)
       }
     }
   }
  
   /*def tamanhoLista(list: List[Int]) : Int  ={
    tamanhoListaAux(list, 0)    
  }
  
  def tamanhoListaAux(list: List[Int], tamAtual: Int) : Int  ={
    if(list.isEmpty) 0
    if(list.tail.isEmpty) 1
    tamanhoListaAux(list.tail, tamAtual +1)    
  }*/
}