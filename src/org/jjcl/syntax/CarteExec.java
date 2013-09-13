package org.jjcl.syntax;

import java.util.List;

public class CarteExec extends AbstractCarte {
	private List<Carte> dds;
	private String program;
	
	@Override
	public void addChild(Carte carte) throws JCLSyntaxException {
		if (carte==null) {
			throw new JCLSyntaxException("Carte null");
		} else if (TypeCarte.DD!=carte.getTypeCarte()) {
			throw new JCLSyntaxException("Une Carte " + carte.getTypeCarte() + " ne peut-être ajoutée à une carte " + this.getTypeCarte() );
		} else {
			this.dds.add(carte);
		}
	}

	public List<Carte> getDds() {
		return dds;
	}

	public String getProgram() {
		return program;
	}		
}
