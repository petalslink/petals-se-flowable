/**
 * Copyright (c) 2015-2019 Linagora
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
import org.flowable.bpmn.model.FlowElement;
import org.flowable.bpmn.model.FormProperty;
import org.flowable.bpmn.model.Process;
import org.flowable.bpmn.model.UserTask;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoProcessInstanceIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoUserIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.NoUserTaskIdMappingException;
import org.ow2.petals.flowable.incoming.operation.annotated.exception.UserTaskIdNotFoundInModelException;
import org.ow2.petals.flowable.incoming.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.flowable.incoming.operation.exception.NoUserIdValueException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingFault;
import org.ow2.petals.flowable.incoming.operation.exception.ProcessInstanceNotFoundException;
import org.ow2.petals.flowable.incoming.operation.exception.TaskCompletedException;
import org.ow2.petals.flowable.incoming.operation.exception.UnexpectedUserException;
import org.ow2.petals.flowable.incoming.variable.VariableDefinition;

/**
 * The BPMN operation 'complete user task' extracted from WDSL according to BPMN annotations. This operation is used to
 * complete a user task of process instance..
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class CompleteUserTaskAnnotatedOperation extends AnnotatedOperationWithOut {

    public static final String BPMN_ACTION = "userTask";

    private static final List<String> EXCEPTIONS_MAPPED;

    static {
        EXCEPTIONS_MAPPED = new ArrayList<>();
        for (final Class<OperationProcessingFault> exception : new Class[] { ProcessInstanceNotFoundException.class,
                TaskCompletedException.class, NoProcessInstanceIdValueException.class, NoUserIdValueException.class,
                UnexpectedUserException.class }) {
            EXCEPTIONS_MAPPED.add(exception.getSimpleName());
        }
    }

    /**
     * The identifier of the user task on which the completion action must be realized on the BPMN process side
     */
    private final String userTaskId;

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
     * @param userTaskId
     * @param processInstanceIdHolder
     *            The placeholder of BPMN process instance identifier associated to the BPMN operation. Not
     *            <code>null</code>.
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
    public CompleteUserTaskAnnotatedOperation(final QName wsdlOperationName, final String processDefinitionId,
            final String userTaskId, final XPathExpression processInstanceIdHolder, final XPathExpression userIdHolder,
            final Map<String, VariableDefinition> variables, final Templates outputTemplate,
            final Map<String, Templates> faultTemplates) throws InvalidAnnotationForOperationException {
        super(wsdlOperationName, processDefinitionId, userIdHolder, variables, outputTemplate, faultTemplates, true);
        this.userTaskId = userTaskId;
        this.processInstanceIdHolder = processInstanceIdHolder;
    }

    @Override
    public String getAction() {
        return BPMN_ACTION;
    }

    @Override
    public void doAnnotationCoherenceCheck(final BpmnModel model) throws InvalidAnnotationForOperationException {

        super.doAnnotationCoherenceCheck(model);

        // The mapping defining the user id is required
        if (this.userIdHolder == null) {
            throw new NoUserIdMappingException(this.wsdlOperation);
        }

        // The mapping defining the process instance id is required to complete a user task
        if (this.processInstanceIdHolder == null) {
            throw new NoProcessInstanceIdMappingException(this.wsdlOperation);
        }
    }

    @Override
    protected List<FormProperty> getVariablesFromModel(final BpmnModel model)
            throws InvalidAnnotationForOperationException {

        // The user task identifier is required
        if (this.userTaskId == null || this.userTaskId.trim().isEmpty()) {
            throw new NoUserTaskIdMappingException(this.wsdlOperation);
        }

        final Process process = model.getProcessById(this.getProcessDefinitionId());
        for (final FlowElement flowElt : process.getFlowElements()) {
            // search the user task
            if ((flowElt instanceof UserTask) && (flowElt.getId().equals(this.userTaskId))) {
                final UserTask userTask = (UserTask) flowElt;
                return userTask.getFormProperties();
            }
        }
        throw new UserTaskIdNotFoundInModelException(this.wsdlOperation, this.userTaskId,
                this.getProcessDefinitionId());
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
     * @return The identifier of the user task on which the completion action must be realized on the BPMN process side
     */
    public String getUserTaskId() {
        return this.userTaskId;
    }

}
