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
      assert(m.isomorph(m2.mol, m2.fingerprint))
    }
  }

  property("structure subgraph round trip"){
    forAll{ m: Mol ⇒ 
      val (m2,fp) = (chem readForQuery m.structure.v).toOption.get
      assert(m.hasSubgraph(m2, fp))
    }
  }
}

// vim: set ts=2 sw=2 et:

