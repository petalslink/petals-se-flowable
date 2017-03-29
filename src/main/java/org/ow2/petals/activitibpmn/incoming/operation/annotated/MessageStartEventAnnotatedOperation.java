/**
 * Copyright (c) 2015-2017 Linagora
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
package org.ow2.petals.activitibpmn.incoming.operation.annotated;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.xpath.XPathExpression;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.EventDefinition;
import org.activiti.bpmn.model.FlowElement;
import org.activiti.bpmn.model.FormProperty;
import org.activiti.bpmn.model.Message;
import org.activiti.bpmn.model.MessageEventDefinition;
import org.activiti.bpmn.model.Process;
import org.activiti.bpmn.model.StartEvent;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.NoStartEventMessageNameMappingException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.StartEventMessageDefinitionnNotFoundInModelException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.StartEventMessageNameNotFoundInModelException;

/**
 * The BPMN operation 'none start event' extracted from WDSL according to BPMN annotations. This operation is used to
 * create a new instance of the process from a node 'none start event'.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class MessageStartEventAnnotatedOperation extends StartEventAnnotatedOperation {

    /**
     * The name of the step 'start event message' on which the action must be realize on the BPMN process side
     */
    private final String startEventMessageName;

    /**
     * The tenant identifier in which the process definition is deployed
     */
    protected final String tenantId;

    /**
     * 
     * @param wsdlOperationName
     *            The WSDL operation containing the current annotations
     * @param processDefinitionId
     *            The BPMN process definition identifier associated to the BPMN operation. Not <code>null</code>.
     * @param startEventMessageName
     *            The name of the step 'start event message' on which the action must be realize on the BPMN process
     *            side
     * @param tenantId
     *            The tenant identifier in which the process definition is deployed
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
    public MessageStartEventAnnotatedOperation(final QName wsdlOperationName, final String processDefinitionId,
            final String startEventMessageName, final String tenantId, final XPathExpression userIdHolder,
            final Map<String, XPathExpression> variables,
            final Templates outputTemplate, final Map<String, Templates> faultTemplates)
            throws InvalidAnnotationForOperationException {
        super(wsdlOperationName, processDefinitionId, userIdHolder, variables, outputTemplate, faultTemplates);
        this.startEventMessageName = startEventMessageName;
        this.tenantId = tenantId;
    }

    public String getStartEventMessageName() {
        return this.startEventMessageName;
    }

    public String getTenantId() {
        return this.tenantId;
    }

    @Override
    public void doAnnotationCoherenceCheck(final BpmnModel model) throws InvalidAnnotationForOperationException {

        // The start event message name is required
        if (this.startEventMessageName == null || this.startEventMessageName.trim().isEmpty()) {
            throw new NoStartEventMessageNameMappingException(this.wsdlOperation);
        }

        // The mapping defining the action identifier must be declared in the process definition
        boolean isMessageNameFound = false;
        List<FormProperty> formPropertyList = null;
        final Process process = model.getProcessById(this.getProcessDefinitionId());
        final Iterator<FlowElement> itFlowElts = process.getFlowElements().iterator();
        while (!isMessageNameFound && itFlowElts.hasNext()) {
            final FlowElement flowElt = itFlowElts.next();
            // search the Start Event Message step
            if (flowElt instanceof StartEvent) {
                final StartEvent startEvent = (StartEvent) flowElt;
                for (final EventDefinition eventDefinition : startEvent.getEventDefinitions()) {
                    if (eventDefinition instanceof MessageEventDefinition) {
                        final String messageRef = ((MessageEventDefinition) eventDefinition).getMessageRef();
                        if (messageRef != null) {
                            final Message message = model.getMessage(messageRef);
                            if (message == null) {
                                throw new StartEventMessageDefinitionnNotFoundInModelException(this.getWsdlOperation(),
                                        this.startEventMessageName, messageRef, this.getProcessDefinitionId());
                            } else if (message.getName().equals(this.startEventMessageName)) {
                                isMessageNameFound = true;
                                formPropertyList = startEvent.getFormProperties();
                            }
                        }
                    }
                }
            }
        }
        
        if (!isMessageNameFound) {
            throw new StartEventMessageNameNotFoundInModelException(this.getWsdlOperation(), this.startEventMessageName,
                    this.getProcessDefinitionId());
        } else {
            if (formPropertyList != null && !formPropertyList.isEmpty()) {
                for (final FormProperty formPropertie : formPropertyList) {
                    // add the FormProperty to the Map <bpmnvar, FormProperty>
                    this.getVariableTypes().put(formPropertie.getId(), formPropertie);
                }
            }
        }
    }

}
