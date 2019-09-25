/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package chem

class MolTest extends CyBySuite with Generators {
  property("smiles generator test"){
    forAll(smilesGen){ s ⇒ assert(Mol.read(s).nonEmpty) }
  }

  property("structure isomorphism round trip"){
    forAll{ m: Mol ⇒ 
      val m2: Mol = Mol.read(m.structure.v).get
      assert(m2 == m)
    }
  }

  property("structure subgraph round trip"){
    forAll{ m: Mol ⇒ 
      val q = QueryMol.read(m.structure.v).get
      assert(q isSubgraphOf m)
    }
  }
}

// vim: set ts=2 sw=2 et:

