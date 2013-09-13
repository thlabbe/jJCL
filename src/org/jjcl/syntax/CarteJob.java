package org.jjcl.syntax;

import java.util.List;

public class CarteJob extends AbstractCarte {
	private List<Carte> steps;

	@Override
	public void addChild(Carte carte) throws JCLSyntaxException {
		if (carte==null) {
			throw new JCLSyntaxException("Carte null");
		} else if (TypeCarte.EXEC!=carte.getTypeCarte()) {
			throw new JCLSyntaxException("Une Carte " + carte.getTypeCarte() + " ne peut-être ajoutée à une carte " + this.getTypeCarte() );
		} else {
			this.steps.add(carte);
		}
	}

}
