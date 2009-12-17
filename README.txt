= nl

nl is a python library that provides a production system with an API modelled on the natural language.

More info can be found in http://bitbucket.org/enriquepablo/nl/wiki/Home.

To install, see INSTALL.txt in this same directory.


intro: theory

nl es un sistema declarativo, en el que se introducen frases y reglas de producción, y del que se extraen respuestas. El principal objetivo es acercarse a la sintaxis y poder expresivo del ln.

Hay diversos sistemas con un objetivo parecido o relacionado. Intentaré introducir nl a través de un pequeño repaso a estos sistemas precedentes.

Lógica de primer orden.
La lógica de primer orden (LPO) es el primer antecedente de nl que vamos a considerar. La LPO es la culminación de un largo proceso filosófico y matemático, previo al desarrollo las primeras computadoras, y tiene una serie de característics que no la hacen apropiada para ser usada mecánicamente por las mismas.
Estática: no aplicable a procesos en tiempo real. -> monotónica.
Tendencia al infinito. A considerar universos infinitos, en conjuntos infinitos de símbolos y axiomas.
Excesivamente prolija si se usan sus reglas de inferencia de una manera mecánica.
Interpretada en universos matemáticos inmutables.
Pensada para analizar a mano estos universos, eternos, para asegurar teoremas manualmente.

Lógicas no monotónicas.
Con la aparición de las computadoras, se tiende a eliminar verbosidad y a mantener conjuntos de verdades que son función del tiempo real.
Cláusulas de horn -> parsimonia.

hay dos avenidas básicas iniciales:

Forward chaining, o sistemas productivos. Un ejemplo es CLIPS. Con estos sistemas se pueden modelar únicamente universos finitos, ya que una vez extendidos, contienen todas las verdades posibles en su dominio de interpretación. Por ejemplo, la aritmética de peano no se puede exresar en estos sistemas, pues la definición de sucesión generaría una productividad ilimitada.

Backward chaining. El ejemplo más ilustre es prolog. Con estos sistemas se pueden modelar universos infinitos. El problema con ellos es la necesidad de símbolos extralógicos (en prolog, el cut) para evitar recursiones infinitas. Por ejemplo, no se puede expresar la transitividad de la relación de subconjunto en una teoría conjuntista sin recurrir al cut:

subset(S1, S3) :- subset(S1, S2) , subset(S2, S3).

Estos sistemas se han usado para producir teorías especializadas, con un campo semántico estrecho, máquinas de estado capaces de simular procesos.
Interpretadas en dominios cerrados, en procesos específicos.

El último (a lo mejor penúltimo) tipo de sistemas que han aparecido son los DL.
Son básicamente teorías conjuntistas, distintas axiomatizaciones de un patrón basico común centrado en las reloaciones de pertenencia y subconjunto. El descbrimiento es que embebiendo esas relaciones básicas en la implementación del sistema se puede conseguir un sistema mucho más expresivo y potente. Se reduce mucho la prolijidad.

Estos sistemas, de nuevo, se usan para producir teorías interpretadas en dominios especializados. 

Sobre las DL se ha creado la web semántica.
Se trata de producir un sistema lógico en red que sea capaz de hablar y razonar sobre los recursos disponibles en esa red.
La interpretación de esta web semántica es algo más complicada. De lo dicho anteriormente se sigue que su dominio de interpretación son los recursos de la red. Pero los recursos de la web son a su vez representaciones de objetos externos (o no) a la web. ¿De qué objetos? De los objetos que forman el universo de discurso del lenguaje natural. Aquellos objetos de los que hablamos son los que representamos en la web. De modo que aquí se podría hablar de un "tetraisomorfismo", si es que existe esa palabra. Tenemos la web semántica, que habla de los recursos de la web, que a su vez representan a objetos extráneos, de los cuales habla el lenguaje natural. La web semántica se quiere para que hable de los reursos de la red de la misma forma en que nosotros hablamos de los objetos que éstos representan. Pero claro, el lenguaje natural no es realmente un isomorfismo de la realidad de que habla, quizá se podría decir todavía; no conocemos la verdad de manera absoluta, la ciencia no ha llegado a un punto en el que se haya dicho: se acabó, ya está terminada, ya lo sabemos todo. De modo que en principio, la web semántica hablaría imperfectamente de lo representado. ¿A dónde quiero llegar con todo ésto? Pues a que se podría considerar que el dominiode interpretación de la web semántica es el propio lenguaje natural. 

Y la idea sería cojer los predicados conjuntistas e interpretarlos en el uso natural del verbo copulativo. Es decir, "juan pertenece a la clase de los hombres" se interpreta en "juan es un hombre", y "hombre es un subconjunto de animal" se interpreta en "un hombre es un animal". A partir de aquí, se añaden nuevos verbos y nombres comunes como predicados y clases en la teoría, con lo que podemos hablar de cualquier cosa; y además, se igualan clases con predicados, al prescribir que para cada podsible predicación, le corresponde una clase anónima.

Curiosamente ése fue el origen de los verbos conjuntistas: un intento de formalizar el uso del lenguaje natural. Durante años, Frege y Russell tuvieron esa idea mientras desarrollaban la LPO.

Paradoja de Russell.

OWL-DL, OWL-Full. Se divorcia la correspondencia con las construcciones del ln de la posibilidad de razonamiento. En OWL-Full es posible expresar la paradoja de Russell.

Salida propuesta por nl. El objetivo es 

predicados sólo los conjuntistas. Los demás verbos se asimilan a operaciones. los hechos perteneces a hecho.

Una lógica muy simple, pero que nos permite decir todo lo necesario.

Symbolic logic, and its culmination in first order logic, was developed before the advent of computers, and is not well suited, in its original form, to be implemented as aseful software. There are mainly two reasons for this inadecuacy.

The first reason is that it is too productive. If all logical connectives and inference rules of classical logic are used in a mechanical way over a set of sentences, we obtain a huge amount of derived sentences, with big sets of equivalent sentences. The reason is that they were developed to produce theorems by hand, with the target theorem in mind, and not to mechanically (by hand or otherwise) produce all possible theorems. Therefore, it was desirable to offer as many roads to any single theorem as possible.

The second reason is that it is monotonic, and therefore it is difficult, if not downright impossible, to represent the concept of "now", which we are forced to contemplate if we consider processes in real time. 

With computers, that spend resources for each of the sentences contained in a theory, we have to try to reduce the cardinality of any set of equivalent classes within a theory to one. Also, with computers we need to represent processes that happen in real time, in a present time that is a moving target from the perspective of physics' (or calendaric) time.

Thus, the symbolic logic implemented as software, usually has two characteristics that distinguish it from classical logic: first, its syntax is reduced to Horn clauses, and its inference rules to modus ponens; and second, they are non-monotonic.

Semantics.
The intended interpretation of the different theories developed with each logic system are in close correspondence with their form. Classical logic was mainly used to study infinite and atemporal mathematical universes (out of real time, even if time was considered in their interpretation). With computers, mathematics is necessarily already in the language used to implement any system; therefore, it would be pointless to develop a logical system intended to implement mathematical theories. Logics in computers is geared towards developing theories interpreted in the real world; is targeted at modelling real world processes.

With respect to the cardinality of the domain of interpretetion, computer logic systems can be classified in forward and backward chaining. FC systems have a closer correspondence with classical logic, but can only consider finite domains of interpretation. BC systems, such as prolog, can consider infinite universes, but the algorithm that implements it can run into infinite loops for certain logical situations, and its grammar is sprinkled with extralogical symbols (such as the cut in prolog) to allow for that.

With respect to their generality, there are systems that implement a general logic with no implied axioms, such as prolog, and there are systems that incorporate some axioms in their implementation. Among the later we can count description logics, that incorporate different axiomatizations of set theory. This can be seen in correspondence to model theory, that demonstrates that set theory is expressive enuogh that can serve as interpretation for any possible first order theory. DLs take this into advantage, and provide within the language the basic predicates of set theory, translated into natural language as the verb "to be" in its use as copular verb. With DLs you produce ontologies, that allow you to be more parsimonious and at the same time comprehensible when talking about any given domain. Closer to the "logic" of the natural language.

With respect to the breadth of its domain, we might classify these systems into closed and open domain. Closed domain would be those systems used to develop theories interpreted in a specific domain, and only interpretable there. In this category we might safely put the majority of the systems. They are used to develop programs, that model certain aspects of certain very specific processes, and can only be interpreted in the domain of those processes.

Open domain systems would be the holy grail of logic in computers. In such a system, a theory would be possible that would be extensible to be able to speak about anything. Like with natural language. This is not to say that naural language can be put in correspondence with a hypothetical formal theory of everything. The natural discourse about reality, with all its truths, possible truths, disputed truths, statistical truths, hermeneutical truths, truths of faith, and half truths, is not in any way consistent. However, within the natural languages there fits theories that while not being formal, are obviously correct, and that are an order of magnitude more expressive than any of the formal theories we have today. In particular, we can easily see that there is the possibility of a perfect and consistent and humanly accessible theory about everything within natural language, but not within any of the formal thories or even ways of building formal theories of today. With theory about everything I mean a consistent set of premises that would allow you to speak about anything you might want to speak about in your mother tongue. The reason for this difference is that in our natural language we can have "first order variables" that range over "predications", (such as in  "whatever he did, he did well"), whereas in a formal language, such a thing is not possible.

An attempt at such an open system / theory is the semantic web. -> Semantics.

OWL DL -> closed domain.

OWL-Full -> open domain, but inconsistent. -> frege.


