package Implementacao
import java.io.File
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
     val inicio = System.currentTimeMillis();
     val input = new BufferedInputStream(new FileInputStream("src/Arquivos/pokerTest.txt")); 
     val br = new BufferedReader(new InputStreamReader(input));
     while(br.ready()){
       val line = br.readLine()
       if(!line.isEmpty()){
         if(quatroIguais(conversion(line.split(" ").toList).collect{ case (a: Int) => (a)}
         .sortBy(x =>x match {case i: Int => i}), 0,0) == false){
           if(sequencia(conversion(line.split(" ").toList).collect{ case (a: Int) => (a)}
         .sortBy(x =>x match {case i: Int => i}), 0)==false){
             todosDiferentes(conversion(line.split(" ").toList).collect{ case (a: Int) => (a)}
         .sortBy(x =>x match {case i: Int => i}), 0,0)
           }
         }
       }
     }
    br.close()
    input.close()

  
   def conversion (line: List[Any]) ={
         line.map(e => 
         if(e=="T")10
         else if(e=="J")11
         else if(e=="Q")12
         else if(e=="K")13
         else if(e=="A")14
         else if(e=="0")0
         else if(e=="1")1
         else if(e=="2")2
         else if(e=="3")3
         else if(e=="4")4
         else if(e=="5")5
         else if(e=="6")6
         else if(e=="7")7
         else if(e=="8")8
         else if(e=="9")9).tail
     }
  
  def increment_counter(n: Int): Int = { n + 1}
  
  def quatroIguais(mao: List[Int], count4Iguais: Int, countQnt4Iguais: Int): Boolean ={
     if(count4Iguais == 3){
       increment_counter(countQnt4Iguais)
       true
     }else{
       if(mao.length < 2 && count4Iguais < 3){
         false
       }else {
         if(mao.head == mao.tail.head){
           quatroIguais(mao.tail, count4Iguais + 1, countQnt4Iguais)
         }else{
           quatroIguais(mao.tail, count4Iguais, countQnt4Iguais)
         }
       }
     }
   }
  
  def sequencia(mao: List[Int], countQntSequencia: Int): Boolean ={
    if(mao.length < 2){
      true
    }else{
      if(mao.head != mao.tail.head-1){
      false
    }else{
      sequencia(mao.tail,countQntSequencia)
    }
  }
 }
  
  def todosDiferentes(mao: List[Int], countQntDiferentes: Int, countQntNadas: Int): Boolean ={
     if(mao.length<2){
       increment_counter(countQntDiferentes)
       true
     }else{
       if(mao.head == mao.tail.head){
         increment_counter(countQntNadas)
         false
       }else {
         todosDiferentes(mao.tail, countQntDiferentes, countQntNadas)
       }
     }
   }
  }
}