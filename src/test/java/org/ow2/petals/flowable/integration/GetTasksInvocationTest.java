/**
 * Copyright (c) 2014-2022 Linagora
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
package org.ow2.petals.flowable.integration;

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETTASKS;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_SERVICE;

import java.util.GregorianCalendar;
import java.util.List;

import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.transform.Source;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.components.flowable.generic._1.GetTasks;
import org.ow2.petals.components.flowable.generic._1.GetTasksResponse;
import org.ow2.petals.components.flowable.generic._1.Task;
import org.ow2.petals.components.flowable.generic._1.Variable;
import org.ow2.petals.flowable.incoming.integration.GetTasksOperation;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Demande;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Numero;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of the service {@link GetTasksOperation}
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class GetTasksInvocationTest extends AbstractIntegrationServiceInvokations {

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is not compliant to the XML schema defined in WSDL</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    @Test
    public void invalidRequest_WsdlUncompliantRequest() throws Exception {

        this.testInvalidRequest_WsdlUncompliant(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                ITG_OP_GETTASKS);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is compliant to the XML schema defined in WSDL</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    @Test
    public void invalidRequest_WsdlCompliantRequest() throws Exception {

        // We use a response as request
        this.testInvalidRequest_WsdlCompliant(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                ITG_OP_GETTASKS, new GetTasksResponse());
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * </p>
     * <ul>
     * <li>an empty request is sent</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    @Test
    public void invalidRequest_EmptyRequest() throws Exception {

        this.testInvalidRequest_Empty(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE, ITG_OP_GETTASKS);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * </p>
     * <ul>
     * <li>no argument is given</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>no fault occurs</li>
     * <li>no task returns because no process instance exists</li>
     * </ul>
     */
    @Test
    public void noArguments() throws Exception {

        final Object getTasksRespObj = this.testRequest(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                ITG_OP_GETTASKS, new GetTasks());

        assertTrue(getTasksRespObj instanceof GetTasksResponse);
        final GetTasksResponse getTasksResp = (GetTasksResponse) getTasksRespObj;
        assertNotNull(getTasksResp.getTasks());
        assertNotNull(getTasksResp.getTasks().getTask());
        assertEquals(0, getTasksResp.getTasks().getTask().size());
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * </p>
     * <ul>
     * <li>valid arguments are given to retrieve a task instance</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>the task instance is retrieved</li>
     * </ul>
     */
    @Test
    public void validRequest() throws Exception {

        // --------------------------------------------------------
        // ---- Create a new instance of the process definition
        // --------------------------------------------------------
        final Demande requestBean_1 = new Demande();
        final String expectedEmployeeName = BPMN_USER_DEMANDEUR;
        requestBean_1.setDemandeur(expectedEmployeeName);
        final int expectedNumberOfDays = 10;
        requestBean_1.setNbJourDde(expectedNumberOfDays);
        final GregorianCalendar now = new GregorianCalendar();
        final GregorianCalendar startDate = new GregorianCalendar(now.get(GregorianCalendar.YEAR),
                now.get(GregorianCalendar.MONTH), now.get(GregorianCalendar.DAY_OF_MONTH));
        final XMLGregorianCalendar expectedStartDate = DatatypeFactory.newInstance().newXMLGregorianCalendar(startDate);
        requestBean_1.setDateDebutDde(expectedStartDate);
        final String expectedMotivation = "hollidays";
        requestBean_1.setMotifDde(expectedMotivation);

        // Send the 1st valid request for start event 'request
        final RequestToProviderMessage request_1 = new RequestToProviderMessage(COMPONENT_UNDER_TEST, VACATION_SU,
                OPERATION_DEMANDERCONGES, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                toByteArray(requestBean_1));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = COMPONENT.sendAndGetResponse(request_1);

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = UNMARSHALLER.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        // --------------------------------------------------------
        // ---- Retrieve task without process variables
        // --------------------------------------------------------
        {
            final GetTasks getTasks = new GetTasks();
            getTasks.setActive(Boolean.TRUE);
            getTasks.setProcessDefinitionIdentifier(BPMN_PROCESS_DEFINITION_KEY);
            getTasks.setProcessInstanceIdentifier(response_1.getNumeroDde());
            getTasks.setTaskDefinitionIdentifier(BPMN_PROCESS_1ST_USER_TASK_KEY);
            final Object getTasksRespObj = this.testRequest(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                    ITG_OP_GETTASKS, getTasks);

            assertTrue(getTasksRespObj instanceof GetTasksResponse);
            final GetTasksResponse getTasksResp = (GetTasksResponse) getTasksRespObj;
            assertNotNull(getTasksResp.getTasks());
            assertNotNull(getTasksResp.getTasks().getTask());
            assertEquals(1, getTasksResp.getTasks().getTask().size());
            final Task task = getTasksResp.getTasks().getTask().get(0);
            assertEquals(BPMN_PROCESS_DEFINITION_KEY, task.getProcessDefinitionIdentifier());
            assertEquals(response_1.getNumeroDde(), task.getProcessInstanceIdentifier());
            assertEquals(BPMN_PROCESS_1ST_USER_TASK_KEY, task.getTaskIdentifier());
            assertEquals("Handle vacation request", task.getTaskName());
            assertEquals(String.format("%s would like to take %d day(s) of vacation (Motivation: %s).",
                    expectedEmployeeName, expectedNumberOfDays, expectedMotivation), task.getTaskDescription());
        }

        // --------------------------------------------------------
        // ---- Retrieve task with process variables
        // --------------------------------------------------------
        {
            final GetTasks getTasks = new GetTasks();
            getTasks.setActive(Boolean.TRUE);
            getTasks.setProcessDefinitionIdentifier(BPMN_PROCESS_DEFINITION_KEY);
            getTasks.setProcessInstanceIdentifier(response_1.getNumeroDde());
            getTasks.setTaskDefinitionIdentifier(BPMN_PROCESS_1ST_USER_TASK_KEY);
            getTasks.setWithProcessInstanceVariables(Boolean.TRUE);
            final Object getTasksRespObj = this.testRequest(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                    ITG_OP_GETTASKS, getTasks);

            assertTrue(getTasksRespObj instanceof GetTasksResponse);
            final GetTasksResponse getTasksResp = (GetTasksResponse) getTasksRespObj;
            assertNotNull(getTasksResp.getTasks());
            assertNotNull(getTasksResp.getTasks().getTask());
            assertEquals(1, getTasksResp.getTasks().getTask().size());
            final Task task = getTasksResp.getTasks().getTask().get(0);
            assertEquals(BPMN_PROCESS_DEFINITION_KEY, task.getProcessDefinitionIdentifier());
            assertEquals(response_1.getNumeroDde(), task.getProcessInstanceIdentifier());
            assertEquals(BPMN_PROCESS_1ST_USER_TASK_KEY, task.getTaskIdentifier());
            assertEquals("Handle vacation request", task.getTaskName());
            assertEquals(String.format("%s would like to take %d day(s) of vacation (Motivation: %s).",
                    expectedEmployeeName, expectedNumberOfDays, expectedMotivation), task.getTaskDescription());

            assertNotNull(task.getProcessVariables());
            final List<Variable> variables = task.getProcessVariables().getVariable();
            assertFalse(variables.isEmpty());
            boolean employeeNameFound = false;
            boolean numberOfDaysFound = false;
            boolean startDateFound = false;
            boolean vacationMotivationFound = false;
            for (final Variable variable : variables) {
                if (variable.getName().equals("employeeName")) {
                    assertEquals(expectedEmployeeName, variable.getValue());
                    employeeNameFound = true;
                } else if (variable.getName().equals("numberOfDays")) {
                    assertEquals(String.valueOf(expectedNumberOfDays), variable.getValue());
                    numberOfDaysFound = true;
                } else if (variable.getName().equals("startDate")) {
                    assertEquals(expectedStartDate.toString(), variable.getValue());
                    startDateFound = true;
                } else if (variable.getName().equals("vacationMotivation")) {
                    assertEquals(expectedMotivation, variable.getValue());
                    vacationMotivationFound = true;
                }
            }
            assertTrue(employeeNameFound);
            assertTrue(numberOfDaysFound);
            assertTrue(startDateFound);
            assertTrue(vacationMotivationFound);
        }
    }

}
