/**
 * Copyright (c) 2015-2021 Linagora
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

import org.flowable.bpmn.model.BpmnModel;
import org.flowable.bpmn.model.FlowElement;
import org.flowable.bpmn.model.FormProperty;
import org.flowable.bpmn.model.Process;
import org.flowable.bpmn.model.StartEvent;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoNoneStartEventIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoneStartEventIdNotFoundInModelException;
import org.ow2.petals.flowable.incoming.variable.VariableDefinition;

/**
 * The BPMN operation 'none start event' extracted from WDSL according to BPMN annotations. This operation is used to
 * create a new instance of the process from a node 'none start event'.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class NoneStartEventAnnotatedOperation extends StartEventAnnotatedOperation {

    /**
     * The identifier of the step 'none start event' on which the action must be realize on the BPMN process side
     */
    private final String noneStartEventId;

    /**
     * 
     * @param wsdlOperationName
     *            The WSDL operation containing the current annotations
     * @param processDefinitionId
     *            The BPMN process definition identifier associated to the BPMN operation. Not <code>null</code>.
     * @param noneStartEventId
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
    public NoneStartEventAnnotatedOperation(final QName wsdlOperationName, final String processDefinitionId,
            final String noneStartEventId, final XPathExpression userIdHolder,
            final Map<String, VariableDefinition> variables, final Templates outputTemplate,
            final Map<String, Templates> faultTemplates) throws InvalidAnnotationForOperationException {
        super(wsdlOperationName, processDefinitionId, userIdHolder, variables, outputTemplate, faultTemplates);
        this.noneStartEventId = noneStartEventId;
    }

    public String getNoneStartEventId() {
        return this.noneStartEventId;
    }

    @Override
    public void doAnnotationCoherenceCheck(final BpmnModel model) throws InvalidAnnotationForOperationException {

        super.doAnnotationCoherenceCheck(model);

        // The start event identifier is required
        if (this.noneStartEventId == null || this.noneStartEventId.trim().isEmpty()) {
            throw new NoNoneStartEventIdMappingException(this.wsdlOperation);
        }
    }

    @Override
    protected List<FormProperty> getVariablesFromModel(final BpmnModel model)
            throws InvalidAnnotationForOperationException {
        final Process process = model.getProcessById(this.getProcessDefinitionId());
        for (final FlowElement flowElt : process.getFlowElements()) {
            // search the None Start Event step
            if ((flowElt instanceof StartEvent) && (flowElt.getId().equals(this.noneStartEventId))
                    && ((StartEvent) flowElt).getEventDefinitions().isEmpty()) {
                final StartEvent startEvent = (StartEvent) flowElt;
                return startEvent.getFormProperties();
            }
        }
        throw new NoneStartEventIdNotFoundInModelException(this.getWsdlOperation(), this.noneStartEventId,
                this.getProcessDefinitionId());
    }

}
