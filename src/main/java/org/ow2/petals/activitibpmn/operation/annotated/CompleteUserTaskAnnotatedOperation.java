/**
 * Copyright (c) 2014 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.operation.annotated;

import java.util.Properties;
import java.util.Set;

import javax.xml.xpath.XPathExpression;

import org.ow2.petals.activitibpmn.operation.annotated.exception.InvalidAnnotationForOperationException;
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoProcessInstanceIdMappingException;

/**
 * The BPMN operation 'complete user task' extracted from WDSL according to BPMN annotations. This operation is used to
 * complete a user task of process instance..
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class CompleteUserTaskAnnotatedOperation extends AnnotatedOperation {

    public static final String BPMN_ACTION_TYPE = "userTask";

    /**
     * 
     * @param wsdlOperationName
     *            The WSDL operation containing the current annotations
     * @param processDefinitionId
     *            The BPMN process definition identifier associated to the BPMN operation. Not <code>null</code>.
     * @param bpmnAction
     * @param processInstanceIdHolder
     *            The placeholder of BPMN process instance identifier associated to the BPMN operation. Not
     *            <code>null</code>.
     * @param userIdHolder
     *            The placeholder of BPMN user identifier associated to the BPMN operation. Not <code>null</code>.
     * @param bpmnVarInMsg
     * @param outMsgBpmnVar
     * @param faultMsgBpmnVar
     * @param bpmnVarList
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    public CompleteUserTaskAnnotatedOperation(final String wsdlOperationName, final String processDefinitionId,
            final String bpmnAction, final XPathExpression processInstanceIdHolder, final XPathExpression userIdHolder,
            final Properties bpmnVarInMsg, final Properties outMsgBpmnVar, final Properties faultMsgBpmnVar,
            final Set<String> bpmnVarList) throws InvalidAnnotationForOperationException {
        super(wsdlOperationName, processDefinitionId, bpmnAction, processInstanceIdHolder, userIdHolder, bpmnVarInMsg,
                outMsgBpmnVar, faultMsgBpmnVar, bpmnVarList);
    }

    @Override
    public String getBpmnActionType() {
        return BPMN_ACTION_TYPE;
    }

    @Override
    protected void verifyAnnotationCoherence() throws InvalidAnnotationForOperationException {

        super.verifyAnnotationCoherence();

        // The mapping defining the process instance id is required to complete a user task
        final XPathExpression processInstanceIdHolder = this.getProcessInstanceIdHolder();
        if (processInstanceIdHolder == null) {
            throw new NoProcessInstanceIdMappingException(this.getWsdlOperationName());
        }

    }

}
