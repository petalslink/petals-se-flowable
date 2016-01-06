/**
 * Copyright (c) 2015-2016 Linagora
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
package org.ow2.petals.activitibpmn.incoming.operation.annotated;

import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.xpath.XPathExpression;

import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FormProperty;
import org.activiti.bpmn.model.StartEvent;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.ActionIdNotFoundInModelException;
import org.ow2.petals.activitibpmn.incoming.operation.annotated.exception.InvalidAnnotationForOperationException;

/**
 * The BPMN operation 'start event' extracted from WDSL according to BPMN annotations. This operation is used to create
 * a new instance of the process.
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class StartEventAnnotatedOperation extends AnnotatedOperation {

    public static final String BPMN_ACTION = "startEvent";

    /**
     * 
     * @param wsdlOperationName
     *            The WSDL operation containing the current annotations
     * @param processDefinitionId
     *            The BPMN process definition identifier associated to the BPMN operation. Not <code>null</code>.
     * @param actionId
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
    public StartEventAnnotatedOperation(final QName wsdlOperationName, final String processDefinitionId,
            final String actionId, final XPathExpression processInstanceIdHolder, final XPathExpression userIdHolder,
            final Map<String, XPathExpression> variables, final Templates outputTemplate,
            final Map<String, Templates> faultTemplates) throws InvalidAnnotationForOperationException {
        super(wsdlOperationName, processDefinitionId, actionId, processInstanceIdHolder, userIdHolder, variables,
                outputTemplate, faultTemplates);
    }

    @Override
    public void doAnnotationCoherenceCheck(final BpmnModel model) throws InvalidAnnotationForOperationException {

        // The mapping defining the action identifier must be declared in the process definition
        boolean isActionIdFound = false;
        List<FormProperty> formPropertyList = null;
        outerloop: for (final org.activiti.bpmn.model.Process process : model.getProcesses()) {
            for (final org.activiti.bpmn.model.FlowElement flowElt : process.getFlowElements()) {
                // search the Start Event: bpmnAction
                if ((flowElt instanceof StartEvent) && (flowElt.getId().equals(this.getActionId()))) {
                    final StartEvent startEvent = (StartEvent) flowElt;
                    formPropertyList = startEvent.getFormProperties();
                    isActionIdFound = true;
                    break outerloop;
                }
            }
        }
        if (!isActionIdFound) {
            throw new ActionIdNotFoundInModelException(this.getWsdlOperation(), this.getActionId(),
                    this.getProcessDefinitionId());
        } else {
            if (formPropertyList != null && formPropertyList.size() > 0) {
                for (final FormProperty formPropertie : formPropertyList) {
                    // add the FormProperty to the Map <bpmnvar, FormProperty>
                    this.getVariableTypes().put(formPropertie.getId(), formPropertie);
                }
            }
        }
    }

    @Override
    public String getAction() {
        return BPMN_ACTION;
    }

    @Override
    protected void addMappedExceptionNames(final List<String> mappedExceptionNames) {
        // TODO Auto-generated method stub
    }

}
