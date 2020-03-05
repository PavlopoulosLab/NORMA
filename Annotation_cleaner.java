/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

/**
 *
 * @author Mikaela
 */
public class Annotation_cleaner {

    static String file_network = "string_interactions_PAP_new.txt";
    static String file_annotation = "PAP_new_BP.txt";

    static ArrayList<String> dictionary = new ArrayList();
    static ArrayList<String> words_to_be_removed = new ArrayList();

    public static void main(String[] args) {
        read_dictionary(file_network);
        read_annotations(file_annotation);
    }

    static void read_annotations(String filename) {
         try {
            File myObj = new File(filename);
            Scanner myReader = new Scanner(myObj);
            StringBuffer buf_annotation = new StringBuffer();
            StringBuffer buf = new StringBuffer();
            while (myReader.hasNextLine()) {
                buf_annotation.setLength(0);
                String data = myReader.nextLine();
                String[] str = data.split("\t");
                String annotation = str[1];
                String[] sss = annotation.split(",");
                for (int i = 0; i < sss.length; i++) {
                    String ss = sss[i].trim().replaceAll("\"", "");
                    if (ss.length() > 0) {
                        if (!dictionary.contains(ss)) {
                            if (!words_to_be_removed.contains(ss)) {
                                words_to_be_removed.add(ss);
                            }
                        }
                    }
                }               
            }
            myReader.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
         
         
        try {
            File myObj = new File(filename);
            Scanner myReader = new Scanner(myObj);
            StringBuffer buf_annotation = new StringBuffer();
            StringBuffer buf = new StringBuffer();
            while (myReader.hasNextLine()) {
                buf_annotation.setLength(0);
                String data = myReader.nextLine();
                String[] str = data.split("\t");
                String annotation = str[1];
                String[] sss = annotation.split(",");
                for (int i = 0; i < sss.length; i++) {
                    if (!words_to_be_removed.contains(sss[i].trim().replaceAll("\"", ""))) {
                        buf_annotation.append(sss[i].trim().replaceAll("\"", "") + ",");
                    }
                }
                annotation = buf_annotation.toString();
                if(annotation.length()>0)
                buf.append(str[0]+"\t"+annotation.substring(0,annotation.length()-1)+"\n");
            }
            myReader.close();
            System.out.println(buf);
            write_to_file(buf, filename + "_cleaned.txt");
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

    static void read_dictionary(String filename) {
        try {
            int cnt = 0;
            File myObj = new File(filename);
            Scanner myReader = new Scanner(myObj);
            while (myReader.hasNextLine()) {
                String data = myReader.nextLine();
                if (cnt > 0) {
                    String[] str = data.split("\t");
                    if (!dictionary.contains(str[0])) {
                        dictionary.add(str[0]);
                    }
                    if (!dictionary.contains(str[1])) {
                        dictionary.add(str[1]);
                    }
                }
                cnt++;
            }
            myReader.close();
            System.out.println(dictionary.size());
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
    }

    static void write_to_file(StringBuffer buf, String filename) {
        try {
            FileWriter myWriter = new FileWriter(filename);
            myWriter.write(buf.toString());
            System.out.println("Wrote file successfully");
            myWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
