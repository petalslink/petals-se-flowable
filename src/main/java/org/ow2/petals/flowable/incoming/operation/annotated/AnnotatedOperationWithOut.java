/**
 * Copyright (c) 2017-2021 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.incoming.operation.annotated;

import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.xpath.XPathExpression;

import org.flowable.bpmn.model.BpmnModel;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoOutputMappingException;
import org.ow2.petals.flowable.incoming.variable.VariableDefinition;

public abstract class AnnotatedOperationWithOut extends AnnotatedOperation {

    /**
     * The output XSLT style-sheet compiled
     */
    private final Templates outputTemplate;

    public AnnotatedOperationWithOut(final QName wsdlOperation, final String processDefinitionId,
            final XPathExpression userIdHolder, final Map<String, VariableDefinition> variables,
            final Templates outputTemplate, final Map<String, Templates> faultTemplates,
            final boolean canDeclareVariable)
            throws InvalidAnnotationForOperationException {
        super(wsdlOperation, processDefinitionId, userIdHolder, variables, faultTemplates, canDeclareVariable);

        this.outputTemplate = outputTemplate;
    }


    @Override
    public void doAnnotationCoherenceCheck(final BpmnModel model) throws InvalidAnnotationForOperationException {

        // The mapping defining the output XSLT style-sheet is required
        if (this.outputTemplate == null) {
            throw new NoOutputMappingException(this.wsdlOperation);
        }
    }

    /**
     * @return The output XSLT style-sheet compiled
     */
    public Templates getOutputTemplate() {
        return this.outputTemplate;
    }

}
