import org.semanticweb.HermiT.Reasoner;
import org.semanticweb.owlapi.apibinding.OWLManager;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.DefaultPrefixManager;
import org.semanticweb.owlapi.util.SimpleShortFormProvider;

import java.io.File;
import java.io.IOException;

public class Main {
        public static void main(String[] args) throws IOException, InterruptedException, OWLOntologyStorageException {
            String prefix = "http://www.co-ode.org/ontologies/pizza/pizza.owl#";

            PrefixManager pr = new DefaultPrefixManager(prefix);

            OWLOntologyManager m=OWLManager.createOWLOntologyManager();
            // We use the OWL API to load the Pizza ontology.
            OWLOntology o;
            try {
                o = m.loadOntologyFromOntologyDocument(new File("/home/emanuele/Documenti/terzo_anno/tesi/hermit/project/examples/ontologies/pizza.owl"));
            } catch (OWLOntologyCreationException e) {
                throw new RuntimeException(e);
            }
            // Now, we instantiate HermiT by creating an instance of the Reasoner class in the package org.semanticweb.HermiT.

            OWLDataFactory df = m.getOWLDataFactory();

            String inizio = args[0];
            String [] diviso = inizio.split(":");
            OWLClass [] vars = new OWLClass[diviso.length / 2];

            // VARIABILI
            for (int i = 0; i < diviso.length; i+=2) {
                OWLClass tmp = df.getOWLClass(IRI.create(prefix+ diviso[i].trim()));
                m.addAxiom(o, df.getOWLDeclarationAxiom(tmp));
                vars[i/2] = tmp;
            }

            DLQueryParser parser =  new DLQueryParser(o,new SimpleShortFormProvider());
            for (int i = 1; i < diviso.length; i+=2) {
                OWLClassExpression classExpression = parser.parseClassExpression(diviso[i].trim());
                m.addAxiom(o , df.getOWLSubClassOfAxiom(vars[(i-1)/2], classExpression));
            }

            Reasoner hermit=new Reasoner(o);

            boolean sat = true;
            for (var v:
                    vars) {
                sat &= hermit.isSatisfiable(v);
            }
            System.out.println(sat);

    /*
    OWLAxiom ax = df.getOWLSubClassOfAxiom(x, classExpression);
    m.addAxiom(o, ax);
    System.out.println(hermit.isSatisfiable(df.getOWLClass("x", pr)));
     */
    /*

    OWLClass X = df.getOWLClass(IRI.create(prefix+ diviso[0].trim()));
    OWLClass Y = df.getOWLClass(IRI.create(prefix+ diviso[2].trim()));
    m.addAxiom(o, df.getOWLDeclarationAxiom(X));
    m.addAxiom(o, df.getOWLDeclarationAxiom(Y));

    DLQueryParser parser =  new DLQueryParser(o,new SimpleShortFormProvider());
    OWLClassExpression classExpressionX = parser.parseClassExpression(diviso[1].trim());
    OWLClassExpression classExpressionY = parser.parseClassExpression(diviso[3].trim());
    m.addAxiom(o , df.getOWLSubClassOfAxiom(X , classExpressionX));
    m.addAxiom(o , df.getOWLSubClassOfAxiom(Y , classExpressionY));

    Reasoner hermit=new Reasoner(o);

    System.out.println(hermit.isSatisfiable(X));
    System.out.println(hermit.isSatisfiable(Y));

    * */
        }

}