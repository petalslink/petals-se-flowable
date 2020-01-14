/**
 * Copyright (c) 2015-2020 Linagora
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
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.xpath.XPathExpression;

import org.flowable.bpmn.model.BpmnModel;
import org.flowable.bpmn.model.EventDefinition;
import org.flowable.bpmn.model.FlowElement;
import org.flowable.bpmn.model.FormProperty;
import org.flowable.bpmn.model.IntermediateCatchEvent;
import org.flowable.bpmn.model.MessageEventDefinition;
import org.flowable.bpmn.model.Process;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.IntermediateMessageCatchEventIdNotFoundInModelException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.IntermediateMessageEventNameNotFoundInModelException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoIntermediateMessageCatchEventIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoProcessInstanceIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.VariableTypeRequiredException;
import org.ow2.petals.flowable.incoming.operation.exception.ProcessInstanceEndedException;
import org.ow2.petals.flowable.incoming.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingFault;
import org.ow2.petals.flowable.incoming.operation.exception.ProcessInstanceNotFoundException;
import org.ow2.petals.flowable.incoming.operation.exception.UnexpectedMessageEventException;
import org.ow2.petals.flowable.incoming.variable.VariableDefinition;

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
                NoProcessInstanceIdValueException.class, UnexpectedMessageEventException.class,
                ProcessInstanceEndedException.class }) {
            EXCEPTIONS_MAPPED.add(exception.getSimpleName());
        }
    }

    /**
     * The message name associated to the intermediate catch event in the BPMN definition
     */
    private final String messageEventName;

    /**
     * The name of the activity associated to the message event receipt in the BPMN definition
     */
    private String messageCatcherActivityId = null;

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
            final String messageEventName, final Map<String, VariableDefinition> variables,
            final Map<String, Templates> faultTemplates) throws InvalidAnnotationForOperationException {
        super(wsdlOperationName, processDefinitionId, null, variables, faultTemplates, false);
        this.messageEventName = messageEventName;
        this.processInstanceIdHolder = processInstanceIdHolder;
    }

    @Override
    public String getAction() {
        return BPMN_ACTION;
    }

    @Override
    protected List<FormProperty> getVariablesFromModel(final BpmnModel model)
            throws InvalidAnnotationForOperationException {
        // No variable can be defined as form property on an intermediate message catch event element.
        return Collections.EMPTY_LIST;
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
        for (final FlowElement flowElt : process.getFlowElements()) {
            // search the intermediate message catch event
            if (flowElt instanceof IntermediateCatchEvent) {
                final IntermediateCatchEvent intermediateCatchEvent = (IntermediateCatchEvent) flowElt;
                final List<EventDefinition> eventDefinitions = intermediateCatchEvent.getEventDefinitions();
                if (eventDefinitions != null && eventDefinitions.size() == 1
                        && eventDefinitions.get(0) instanceof MessageEventDefinition) {
                    final String messageRef = ((MessageEventDefinition) eventDefinitions.get(0)).getMessageRef();
                    if (this.messageEventName.equals(model.getMessage(messageRef).getName())) {
                        this.messageCatcherActivityId = flowElt.getId();
                        if (this.messageCatcherActivityId == null) {
                            // The identifier of the activity associated to the intermediate message event is missing
                            throw new IntermediateMessageCatchEventIdNotFoundInModelException(this.wsdlOperation,
                                    this.messageEventName, this.getProcessDefinitionId());
                        }
                        break;
                    }
                }
            }
        }
        if (this.messageCatcherActivityId == null) {
            // The message name was not found in the BPMN definition
            throw new IntermediateMessageEventNameNotFoundInModelException(this.wsdlOperation, this.messageEventName,
                    this.getProcessDefinitionId());
        }

        // Variable type is required
        for (final VariableDefinition variable : this.getVariables().values()) {
            if (variable.getType() == null) {
                throw new VariableTypeRequiredException(this.wsdlOperation, variable.getName());
            }
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

    /**
     * @return The message name of the intermediate catch event on which the action must be realized on the BPMN process
     *         side
     */
    public String getMessageCatcherActivityId() {
        return this.messageCatcherActivityId;
    }

}
