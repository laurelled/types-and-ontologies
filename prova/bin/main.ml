type classExpression = 
Property of string 
| ProperyInverse of string
| Atomic of string 
| Top 
| Conjunction of classExpression * classExpression 
| Exist of classExpression * classExpression

type axiom = string * classExpression


let rec stringfy (c : classExpression) : string =
match c with 
| Property s -> s
| ProperyInverse s -> "INVERSE " ^ s 
| Atomic s -> s
| Top -> "T"
| Conjunction (c1 , c2) -> (stringfy c1) ^ " AND " ^ (stringfy c2)
| Exist (c1 , c2) -> (stringfy c1) ^ " SOME " ^ (stringfy c2);;


let rec stringfyAxiom (list : axiom list) : string =
  match list with
  | [] -> ""
  | ( (a,b) :: []) ->  a ^ " : " ^ (stringfy b)
  | ( (a,b) :: (x :: xs)) ->  a ^ " : " ^ (stringfy b) ^ " : " ^ stringfyAxiom (x :: xs);;

(**)
let isHabitated  classEx =
let param = stringfyAxiom classEx in
let tmp_file = Filename.temp_file "" ".txt" in
let _ = Sys.command @@ "java -classpath /home/emanuele/IdeaProjects/HermiTDemo/out/production/HermiTDemo:/home/emanuele/Documenti/terzo_anno/tesi/hermit/HermiT.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/org.semanticweb.HermiT.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/gson-2.10.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/collection-0.7.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-arq-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-iri-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-tdb-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/httpcore-4.4.16.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-base-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-cmds-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-core-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-shex-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-tdb2-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-shacl-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/libthrift-0.17.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/log4j-api-2.19.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/slf4j-api-1.7.36.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/commons-cli-1.5.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/commons-csv-1.9.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/commons-io-2.11.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/httpclient-4.5.13.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/log4j-core-2.19.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/commons-codec-1.15.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jakarta.json-2.0.1.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jsonld-java-0.13.4.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jackson-core-2.14.1.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-rdfpatch-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/commons-lang3-3.12.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-dboe-base-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/commons-compress-1.22.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jcl-over-slf4j-1.7.36.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-dboe-index-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/protobuf-java-3.21.10.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/titanium-json-ld-1.3.1.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/httpclient-cache-4.5.13.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jackson-databind-2.14.1.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-dboe-storage-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-shaded-guava-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/log4j-slf4j-impl-2.19.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-rdfconnection-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jackson-annotations-2.14.1.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-dboe-trans-data-4.7.0.jar:/home/emanuele/Documenti/terzo_anno/tesi/hermit/apache-jena-4.7.0/lib/jena-dboe-transaction-4.7.0.jar Main " ^ 
  "\"" ^ param ^ "\" >" ^ tmp_file in
let chan = open_in tmp_file in
let s = input_line chan in
  close_in chan;
  s;;
   



let ex = 
  ("X",Atomic "Margherita" ) ::
  ("X",  Exist (Property "hasTopping" , Atomic "Y")) ::
  ("Y", Atomic "PizzaTopping") ::
  ("Y",  Exist (ProperyInverse  "hasTopping" , Atomic "X")) ::
  []
 in 
Printf.printf "%s" (isHabitated ex);

let some = Exist (Conjunction(ProperyInverse "a",Property "b"), Top) in some;;