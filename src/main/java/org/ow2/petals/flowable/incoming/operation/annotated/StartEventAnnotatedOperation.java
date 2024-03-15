/**
 * Copyright (c) 2015-2024 Linagora
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

import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.xpath.XPathExpression;

import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.flowable.incoming.variable.VariableDefinition;

/**
 * Abstract operation for all BPMN operation 'start event' extracted from WDSL according to BPMN annotations.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class StartEventAnnotatedOperation extends AnnotatedOperationWithOut {

    public static final String BPMN_ACTION = "startEvent";

    /**
     * 
     * @param wsdlOperationName
     *            The WSDL operation containing the current annotations
     * @param processDefinitionId
     *            The BPMN process definition identifier associated to the BPMN operation. Not <code>null</code>.
     * @param userIdHolder
     *            The placeholder of BPMN user identifier associated to the BPMN operation. Not <code>null</code>.
     * @param variables
     *            The definition of variables of the operation
     * @param outputTemplate
     *            The output XSLT style-sheet compiled
     * @param faultTemplates
     *            The XSLT style-sheet compiled associated to WSDL faults
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    public StartEventAnnotatedOperation(final QName wsdlOperationName, final String processDefinitionId,
            final XPathExpression userIdHolder, final Map<String, VariableDefinition> variables,
            final Templates outputTemplate, final Map<String, Templates> faultTemplates)
            throws InvalidAnnotationForOperationException {
        super(wsdlOperationName, processDefinitionId, userIdHolder, variables, outputTemplate, faultTemplates, true);
    }

    @Override
    public String getAction() {
        return BPMN_ACTION;
    }

    @Override
    protected void addMappedExceptionNames(final List<String> mappedExceptionNames) {
        // No exception mapped
    }

}
