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
package org.ow2.petals.flowable.incoming.operation.annotated;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.xpath.XPathExpression;

import org.flowable.bpmn.model.BpmnModel;
import org.flowable.bpmn.model.EventDefinition;
import org.flowable.bpmn.model.FlowElement;
import org.flowable.bpmn.model.IntermediateCatchEvent;
import org.flowable.bpmn.model.MessageEventDefinition;
import org.flowable.bpmn.model.Process;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.IntermediateMessageCatchEventIdNotFoundInModelException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoIntermediateMessageCatchEventIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoProcessInstanceIdMappingException;
import org.ow2.petals.flowable.incoming.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingFault;
import org.ow2.petals.flowable.incoming.operation.exception.ProcessInstanceNotFoundException;

/**
 * The BPMN operation 'intermediate message catch event' extracted from WDSL according to BPMN annotations. This
 * operation is used to notify a process instance of an event receipt.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class IntermediateMessageCatchEventAnnotatedOperation extends AnnotatedOperation {

    public static final String BPMN_ACTION = "intermediateMessageCatch";

    private static final List<String> EXCEPTIONS_MAPPED;

    static {
        EXCEPTIONS_MAPPED = new ArrayList<>();
        for (final Class<OperationProcessingFault> exception : new Class[] { ProcessInstanceNotFoundException.class,
                NoProcessInstanceIdValueException.class }) {
            EXCEPTIONS_MAPPED.add(exception.getSimpleName());
        }
    }

    /**
     * The message name associated to the intermediate catch event in the BPMN definition
     */
    private final String messageEventName;

    /**
     * The place holder of the incoming request containing the process instance identifier on which the BPMN operation
     * must be executed
     */
    private final XPathExpression processInstanceIdHolder;

    /**
     * 
     * @param wsdlOperationName
     *            The WSDL operation containing the current annotations
     * @param processDefinitionId
     *            The BPMN process definition identifier associated to the BPMN operation. Not <code>null</code>.
     * @param processInstanceIdHolder
     *            The placeholder of BPMN process instance identifier associated to the BPMN operation. Not
     *            <code>null</code>.
     * @param messageEventName
     *            The message name associated to the intermediate catch event in the BPMN model
     * @param variables
     *            The definition of variables of the operation
     * @param faultTemplates
     *            The XSLT style-sheet compiled associated to WSDL faults
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    public IntermediateMessageCatchEventAnnotatedOperation(final QName wsdlOperationName,
            final String processDefinitionId, final XPathExpression processInstanceIdHolder,
            final String messageEventName, final Map<String, XPathExpression> variables,
            final Map<String, Templates> faultTemplates) throws InvalidAnnotationForOperationException {
        super(wsdlOperationName, processDefinitionId, null, variables, faultTemplates);
        this.messageEventName = messageEventName;
        this.processInstanceIdHolder = processInstanceIdHolder;
    }

    @Override
    public String getAction() {
        return BPMN_ACTION;
    }

    @Override
    public void doAnnotationCoherenceCheck(final BpmnModel model) throws InvalidAnnotationForOperationException {

        // The message event identifier is required
        if (this.messageEventName == null || this.messageEventName.trim().isEmpty()) {
            throw new NoIntermediateMessageCatchEventIdMappingException(this.wsdlOperation);
        }

        // The mapping defining the process instance id is required for intermediate message catch event
        if (this.processInstanceIdHolder == null) {
            throw new NoProcessInstanceIdMappingException(this.wsdlOperation);
        }

        // The mapping defining the intermediate message catch event identifier must be declared in the process
        // definition
        final Process process = model.getProcessById(this.getProcessDefinitionId());
        boolean catchEventFound = false;
        for (final FlowElement flowElt : process.getFlowElements()) {
            // search the intermediate message catch event
            if (flowElt instanceof IntermediateCatchEvent) {
                final IntermediateCatchEvent intermediateCatchEvent = (IntermediateCatchEvent) flowElt;
                final List<EventDefinition> eventDefinitions = intermediateCatchEvent.getEventDefinitions();
                if (eventDefinitions != null && eventDefinitions.size() == 1
                        && eventDefinitions.get(0) instanceof MessageEventDefinition) {
                    final String messageRef = ((MessageEventDefinition) eventDefinitions.get(0)).getMessageRef();
                    if (this.messageEventName.equals(model.getMessage(messageRef).getName())) {
                        catchEventFound = true;
                        break;
                    }
                }
            }
        }
        if (!catchEventFound) {
            throw new IntermediateMessageCatchEventIdNotFoundInModelException(this.wsdlOperation, this.messageEventName,
                    this.getProcessDefinitionId());
        }
    }

    @Override
    protected void addMappedExceptionNames(final List<String> mappedExceptionNames) {
        mappedExceptionNames.addAll(EXCEPTIONS_MAPPED);
    }

    /**
     * @return The placeholder of BPMN process instance identifier associated to the BPMN operation.
     */
    public XPathExpression getProcessInstanceIdHolder() {
        return this.processInstanceIdHolder;
    }

    /**
     * @return The message name of the intermediate catch event on which the action must be realized on the BPMN process
     *         side
     */
    public String getMessageEventName() {
        return this.messageEventName;
    }

}
