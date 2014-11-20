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
import org.ow2.petals.activitibpmn.operation.annotated.exception.NoUserIdMappingException;

/**
 * A BPMN operation extracted from WDSL according to BPMN annotations
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class AnnotatedOperation {

    /**
     * The WSDL operation containing the current annotations
     */
    private final String wsdlOperationName;

    /**
     * The BPMN process identifier associated to the BPMN operation.
     */
    private final String processIdentifier;

    private final String bpmnAction;

    /**
     * The place holder of the incoming request containing the process instance identifier on which the BPMN operation
     * must be executed
     */
    private final Properties processInstanceIdHolder;

    /**
     * The place holder of the incoming request containing the user identifier with which the BPMN operation must be
     * executed
     */
    private final XPathExpression userIdHolder;

    private final Properties bpmnVarInMsg;

    private final Properties outMsgBpmnVar;

    private final Properties faultMsgBpmnVar;

    private final Set<String> bpmnVarList;

    /**
     * <p>
     * Create an annotated operation.
     * </p>
     * 
     * <p>
     * <b>Note</b>: If the user identifier placeholder is null or empty, the error {@link NoUserIdMappingException} will
     * be thrown.
     * 
     * @param wsdlOperationName
     *            The WSDL operation containing the current annotations
     * @param processIdentifier
     *            The BPMN process identifier associated to the BPMN operation. Not <code>null</code>.
     * @param bpmnAction
     * @param processInstanceIdHolder
     * @param userIdHolder
     *            The placeholder of BPMN user identifier associated to the BPMN operation. Not <code>null</code>.
     * @param bpmnVarInMsg
     * @param outMsgBpmnVar
     * @param faultMsgBpmnVar
     * @param bpmnVarType
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    protected AnnotatedOperation(final String wsdlOperationName, final String processIdentifier,
            final String bpmnAction, final Properties processInstanceIdHolder, final XPathExpression userIdHolder,
            final Properties bpmnVarInMsg, final Properties outMsgBpmnVar, final Properties faultMsgBpmnVar,
            final Set<String> bpmnVarList) throws InvalidAnnotationForOperationException {
        super();
        this.wsdlOperationName = wsdlOperationName;
        this.processIdentifier = processIdentifier;
        this.bpmnAction = bpmnAction;
        this.processInstanceIdHolder = processInstanceIdHolder;
        this.userIdHolder = userIdHolder;
        this.bpmnVarInMsg = bpmnVarInMsg;
        this.outMsgBpmnVar = outMsgBpmnVar;
        this.faultMsgBpmnVar = faultMsgBpmnVar;
        this.bpmnVarList = bpmnVarList;

        this.verifyAnnotationCoherence();
    }

    /**
     * Verify that annotation read from the WSDL are valid for the operation, otherwise the exception
     * {@link InvalidAnnotationForOperationException} is thrown.
     * 
     * @throws InvalidAnnotationForOperationException
     *             The annotated operation is incoherent.
     */
    protected void verifyAnnotationCoherence() throws InvalidAnnotationForOperationException {

        // The mapping defining the user id is required to complete a user task
        if (this.userIdHolder == null) {
            throw new NoUserIdMappingException(this.wsdlOperationName);
        }

    }

    /**
     * @return The WSDL operation containing the current annotations
     */
    public String getWsdlOperationName() {
        return this.wsdlOperationName;
    }

    /**
     * @return The BPMN process identifier associated to the BPMN operation
     */
    public String getProcessIdentifier() {
        return this.processIdentifier;
    }

    /**
     * @return the bpmnAction
     */
    public String getBpmnAction() {
        return this.bpmnAction;
    }

    /**
     * @return The BPM action type of this BPMN operation
     * @return
     */
    public abstract String getBpmnActionType();

    /**
     * @return the bpmnVarList
     */
    public Set<String> getBpmnVarList() {
        return this.bpmnVarList;
    }

    /**
     * @return the processInstanceIdHolder
     */
    public Properties getProcessInstanceIdHolder() {
        return this.processInstanceIdHolder;
    }

    /**
     * @return the userIdHolder
     */
    public XPathExpression getUserIdHolder() {
        return this.userIdHolder;
    }

    /**
     * @return the bpmnVarInMsg
     */
    public Properties getBpmnVarInMsg() {
        return this.bpmnVarInMsg;
    }

    /**
     * @return the outMsgBpmnVar
     */
    public Properties getOutMsgBpmnVar() {
        return this.outMsgBpmnVar;
    }

    /**
     * @return the faultMsgBpmnVar
     */
    public Properties getFaultMsgBpmnVar() {
        return this.faultMsgBpmnVar;
    }

}
