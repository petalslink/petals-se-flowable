/**
 * Copyright (c) 2014-2015 Linagora
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
package org.ow2.petals.activitibpmn;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETTASKS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_PORT_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_SERVICE;
import static org.ow2.petals.component.framework.junit.Assert.assertMonitProviderBeginLog;
import static org.ow2.petals.component.framework.junit.Assert.assertMonitProviderEndLog;
import static org.ow2.petals.component.framework.junit.Assert.assertMonitProviderFailureLog;

import java.io.ByteArrayInputStream;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.LogRecord;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.bind.DatatypeConverter;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.transform.Source;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.activitibpmn.incoming.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.activitibpmn.incoming.operation.exception.NoUserIdValueException;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.impl.message.WrappedRequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.WrappedResponseToConsumerMessage;
import org.ow2.petals.components.activiti.generic._1.GetTasks;
import org.ow2.petals.components.activiti.generic._1.GetTasksResponse;
import org.ow2.petals.components.activiti.generic._1.Task;
import org.ow2.petals.samples.se_bpmn.archivageservice.Archiver;
import org.ow2.petals.samples.se_bpmn.archivageservice.ArchiverResponse;
import org.ow2.petals.samples.se_bpmn.vacationservice.AckResponse;
import org.ow2.petals.samples.se_bpmn.vacationservice.Demande;
import org.ow2.petals.samples.se_bpmn.vacationservice.DemandeDejaValidee;
import org.ow2.petals.samples.se_bpmn.vacationservice.Numero;
import org.ow2.petals.samples.se_bpmn.vacationservice.NumeroDemandeInconnu;
import org.ow2.petals.samples.se_bpmn.vacationservice.Validation;
import org.ow2.petals.samples.se_bpmn.vacationservice.XslParameter;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class BpmnServicesInvocationTest extends AbstractComponentTest {

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd valid request is sent to complete the waiting user task,</li>
     * <li>a 3rd valid request is sent to complete again the user task already completed by the 2nd request.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request,:
     * <ul>
     * <li>the process instance is correctly finished in the Activiti engine,</li>
     * <li>the 1st user task is completed by the right assignee,</li>
     * <li>the service reply is as expected,</li>
     * <li>service tasks invoked Petals services</li>
     * </ul>
     * </li>
     * <li>on the 3rd request, the right business fault associated to a user task already completed is returned because
     * the process instance is finished.</li>
     * </ul>
     * </p>
     */
    @Test
    public void validStartEventRequest() throws Exception {
        
        this.initIdentities();

        // --------------------------------------------------------
        // ---- Create a new instance of the process definition
        // --------------------------------------------------------
        final Demande request_1 = new Demande();
        request_1.setDemandeur(BPMN_USER_DEMANDEUR);
        final int numberOfDays = 10;
        request_1.setNbJourDde(numberOfDays);
        final GregorianCalendar now = new GregorianCalendar();
        final GregorianCalendar startDate = new GregorianCalendar(now.get(GregorianCalendar.YEAR),
                now.get(GregorianCalendar.MONTH), now.get(GregorianCalendar.DAY_OF_MONTH));
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(startDate));
        final String motivation = "hollidays";
        request_1.setMotifDde(motivation);

        // Send the 1st valid request for start event 'request
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_1.size());
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnServicesInvocationTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        {
            assertEquals(5, response_1.getXslParameter().size());
            boolean userFound = false;
            boolean employeeFound = false;
            boolean numberOfDaysFound = false;
            boolean startDateFound = false;
            boolean motivationFound = false;
            for (final XslParameter xslParameter : response_1.getXslParameter()) {
                if ("user-id".equals(xslParameter.getName().toString())) {
                    userFound = true;
                    assertEquals(BPMN_USER_DEMANDEUR, xslParameter.getValue());
                } else if ("employeeName".equals(xslParameter.getName().toString())) {
                    employeeFound = true;
                    assertEquals(BPMN_USER_DEMANDEUR, xslParameter.getValue());
                } else if ("numberOfDays".equals(xslParameter.getName().toString())) {
                    numberOfDaysFound = true;
                    assertEquals(numberOfDays, Integer.parseInt(xslParameter.getValue()));
                } else if ("startDate".equals(xslParameter.getName().toString())) {
                    startDateFound = true;
                    // Don't use 'assertEquals' because time zones can be different
                    assertTrue(startDate.compareTo(DatatypeConverter.parseDate(xslParameter.getValue())) == 0);
                } else if ("vacationMotivation".equals(xslParameter.getName().toString())) {
                    motivationFound = true;
                    assertEquals(motivation, xslParameter.getValue());
                } else {
                    fail("Unexpected xsl parameter: " + xslParameter.getName().toString());
                }
            }
            assertTrue(userFound);
            assertTrue(employeeFound);
            assertTrue(numberOfDaysFound);
            assertTrue(startDateFound);
            assertTrue(motivationFound);
        }
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Verify the task basket using integration service
        final GetTasks getTasksReq = new GetTasks();
        getTasksReq.setActive(true);
        getTasksReq.setAssignee(BPMN_USER_VALIDEUR);
        getTasksReq.setProcessInstanceIdentifier(response_1.getNumeroDde());

        inMemoryLogHandler.clear();
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.nativeServiceConfiguration, ITG_OP_GETTASKS,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(getTasksReq))));
        {
            final ResponseMessage getTaskRespMsg = BpmnServicesInvocationTest.componentUnderTest
                    .pollResponseFromProvider();
            assertNotNull("No XML payload in response", getTaskRespMsg.getPayload());
            final Object getTaskRespObj = BpmnServicesInvocationTest.unmarshaller
                    .unmarshal(getTaskRespMsg.getPayload());
            assertTrue(getTaskRespObj instanceof GetTasksResponse);
            final GetTasksResponse getTaskResp = (GetTasksResponse) getTaskRespObj;
            assertNotNull(getTaskResp.getTasks());
            assertNotNull(getTaskResp.getTasks().getTask());
            assertEquals(1, getTaskResp.getTasks().getTask().size());
            final Task task = getTaskResp.getTasks().getTask().get(0);
            assertEquals(BPMN_PROCESS_DEFINITION_KEY, task.getProcessDefinitionIdentifier());
            assertEquals(response_1.getNumeroDde(), task.getProcessInstanceIdentifier());
            assertEquals(BPMN_PROCESS_1ST_USER_TASK_KEY, task.getTaskIdentifier());
        }
        // Check MONIT traces about the task basket invocation
        final List<LogRecord> monitLogs_taskBasket = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_taskBasket.size());
        final FlowLogData providerBegin_taskBasket = assertMonitProviderBeginLog(ITG_PORT_TYPE, ITG_SERVICE,
                AbstractComponentTest.componentUnderTest.getNativeEndpointName(ITG_SERVICE), ITG_OP_GETTASKS,
                monitLogs_taskBasket.get(0));
        assertMonitProviderEndLog(providerBegin_taskBasket, monitLogs_taskBasket.get(1));

        // ------------------------------------------------------------------
        // ---- Complete the first user task (validate the vacation request)
        // ------------------------------------------------------------------
        final Validation request_2 = new Validation();
        request_2.setValideur(BPMN_USER_VALIDEUR);
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        {
            // Assert the 1st request sent by Activiti on orchestrated service
            final RequestMessage archiveRequestMsg_1 = BpmnServicesInvocationTest.componentUnderTest
                    .pollRequestFromConsumer();
            final MessageExchange archiveMessageExchange_1 = archiveRequestMsg_1.getMessageExchange();
            assertNotNull(archiveMessageExchange_1);
            assertEquals(ARCHIVE_INTERFACE, archiveMessageExchange_1.getInterfaceName());
            assertEquals(ARCHIVE_SERVICE, archiveMessageExchange_1.getService());
            assertNotNull(archiveMessageExchange_1.getEndpoint());
            assertEquals(ARCHIVE_ENDPOINT, archiveMessageExchange_1.getEndpoint().getEndpointName());
            assertEquals(ARCHIVER_OPERATION, archiveMessageExchange_1.getOperation());
            assertEquals(archiveMessageExchange_1.getStatus(), ExchangeStatus.ACTIVE);
            final Object archiveRequestObj_1 = BpmnServicesInvocationTest.unmarshaller.unmarshal(archiveRequestMsg_1
                    .getPayload());
            assertTrue(archiveRequestObj_1 instanceof Archiver);
            final Archiver archiveRequest_1 = (Archiver) archiveRequestObj_1;
            assertEquals(response_1.getNumeroDde(), archiveRequest_1.getItem());

            // Returns the reply of the service provider to the Activiti service task
            final ArchiverResponse archiverResponse_1 = new ArchiverResponse();
            archiverResponse_1.setItem("value of item");
            archiverResponse_1.setItem2("value of item2");
            final ResponseMessage otherResponse_1 = new WrappedResponseToConsumerMessage(archiveMessageExchange_1,
                    new ByteArrayInputStream(this.toByteArray(archiverResponse_1)));
            BpmnServicesInvocationTest.componentUnderTest.pushResponseToConsumer(otherResponse_1);

            // Assert the status DONE on the message exchange
            final RequestMessage statusDoneMsg_1 = BpmnServicesInvocationTest.componentUnderTest
                    .pollRequestFromConsumer();
            assertNotNull(statusDoneMsg_1);
            // It's the same message exchange instance
            assertSame(statusDoneMsg_1.getMessageExchange(), archiveRequestMsg_1.getMessageExchange());
            assertEquals(statusDoneMsg_1.getMessageExchange().getStatus(), ExchangeStatus.DONE);
        }

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        // TODO: Should we have MONIT traces about the service call on the consumer side ?
        assertEquals(2, monitLogs_2.size());
        final FlowLogData providerBegin = assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE,
                VACATION_ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_2.get(0));
        /*
        final FlowLogData consumerBegin = assertMonitConsumerBeginLog(ARCHIVE_INTERFACE, ARCHIVE_SERVICE,
                ARCHIVE_ENDPOINT, ARCHIVER_OPERATION, providerBegin, monitLogs_2.get(1));
        assertMonitConsumerEndLog(consumerBegin, monitLogs_2.get(2));
        */
        // TODO: Investigate why the user task completion is synchronous with following service tasks
        // TODO: Enable following assertion when the user task completion will be not linked to the following service
        // tasks
        // assertMonitProviderEndLog(providerBegin, monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNull("Unexpected fault", (fault_2 == null ? null : SourceHelper.toString(fault_2)));
        assertNotNull("No XML payload in response", responseMsg_2.getPayload());
        final Object responseObj_2 = BpmnServicesInvocationTest.unmarshaller.unmarshal(responseMsg_2.getPayload());
        assertTrue(responseObj_2 instanceof AckResponse);
        final AckResponse response_2 = (AckResponse) responseObj_2;
        {
            assertEquals(7, response_2.getXslParameter().size());
            boolean userFound = false;
            boolean processInstanceIdFound = false;
            boolean employeeFound = false;
            boolean numberOfDaysFound = false;
            boolean startDateFound = false;
            boolean approvedFound = false;
            boolean motivationFound = false;
            for (final XslParameter xslParameter : response_2.getXslParameter()) {
                if ("process-instance-id".equals(xslParameter.getName().toString())) {
                    processInstanceIdFound = true;
                    assertEquals(response_1.getNumeroDde(), xslParameter.getValue());
                } else if ("user-id".equals(xslParameter.getName().toString())) {
                    userFound = true;
                    assertEquals(BPMN_USER_VALIDEUR, xslParameter.getValue());
                } else if ("employeeName".equals(xslParameter.getName().toString())) {
                    employeeFound = true;
                    assertEquals(BPMN_USER_DEMANDEUR, xslParameter.getValue());
                } else if ("numberOfDays".equals(xslParameter.getName().toString())) {
                    numberOfDaysFound = true;
                    assertEquals(numberOfDays, Integer.parseInt(xslParameter.getValue()));
                } else if ("startDate".equals(xslParameter.getName().toString())) {
                    startDateFound = true;
                    // Don't use 'assertEquals' because time zones can be different
                    assertTrue(startDate.compareTo(DatatypeConverter.parseDate(xslParameter.getValue())) == 0);
                } else if ("vacationApproved".equals(xslParameter.getName().toString())) {
                    approvedFound = true;
                    assertEquals(Boolean.TRUE.toString(), xslParameter.getValue());
                } else if ("vacationMotivation".equals(xslParameter.getName().toString())) {
                    motivationFound = true;
                    assertEquals(motivation, xslParameter.getValue());
                } else {
                    fail("Unexpected xsl parameter: " + xslParameter.getName().toString());
                }
            }
            assertTrue(userFound);
            assertTrue(processInstanceIdFound);
            assertTrue(employeeFound);
            assertTrue(numberOfDaysFound);
            assertTrue(startDateFound);
            assertTrue(approvedFound);
            assertTrue(motivationFound);
        }
        assertProcessInstanceFinished(response_1.getNumeroDde());
        assertUserTaskEnded(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // --------------------------------------------------------
        // ---- Try to complete AGAIN the first user task
        // --------------------------------------------------------
        inMemoryLogHandler.clear();
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 3rd valid request
        final ResponseMessage responseMsg_3 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_3 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_3.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_3.get(0)),
                monitLogs_3.get(1));

        // Check the reply
        assertNull("XML payload in response", responseMsg_3.getPayload());
        final Source fault_3 = responseMsg_3.getFault();
        assertNotNull("No fault returns", fault_3);
        final Object responseObj_3 = BpmnServicesInvocationTest.unmarshaller.unmarshal(fault_3);
        assertTrue(responseObj_3 instanceof DemandeDejaValidee);
        final DemandeDejaValidee response_3 = (DemandeDejaValidee) responseObj_3;
        assertEquals(response_1.getNumeroDde(), response_3.getNumeroDde());
    }

    /**
     * <p>
     * Check the message processing where a request is sent to create a new process instance, where:
     * <ul>
     * <li>the user identifier is missing into the incoming payload.</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>a fault is returned to the caller</li>
     * <li>no process instance is created on the Activiti engine</li>
     * </ul>
     * </p>
     */
    @Test
    public void startEventRequest_NoUserIdValue() throws Exception {

        final int currentProcInstNb = this.getProcessInstanceNumber(BPMN_PROCESS_DEFINITION_KEY);

        // Create the 1st valid request
        final Demande request = new Demande();
        request.setNbJourDde(10);
        request.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request.setMotifDde("hollidays");

        // Send the request
        BpmnServicesInvocationTest.componentUnderTest
                .pushRequestToProvider(new WrappedRequestToProviderMessage(
                        BpmnServicesInvocationTest.serviceConfiguration,
                        OPERATION_DEMANDERCONGES, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                        new ByteArrayInputStream(this.toByteArray(request))));

        // Assert the response of the request
        final ResponseMessage responseMsg = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs.get(0)),
                monitLogs.get(1));

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNotNull("No fault returns", fault);
        final String faultStr = SourceHelper.toString(fault);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg.getPayload());

        assertEquals(currentProcInstNb, this.getProcessInstanceNumber(BPMN_PROCESS_DEFINITION_KEY));
    }

    /**
     * <p>
     * Check the message processing where a request is sent to create a new process instance, where:
     * <ul>
     * <li>the user identifier is empty into the incoming payload.</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>a fault is returned to the caller</li>
     * </ul>
     * </p>
     */
    @Test
    public void startEventRequest_EmptyUserIdValue() throws Exception {

        final int currentProcInstNb = this.getProcessInstanceNumber(BPMN_PROCESS_DEFINITION_KEY);

        // Create the 1st valid request
        final Demande request = new Demande();
        request.setDemandeur("");
        request.setNbJourDde(10);
        request.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request.setMotifDde("hollidays");

        // Send the request
        BpmnServicesInvocationTest.componentUnderTest
                .pushRequestToProvider(new WrappedRequestToProviderMessage(
                        BpmnServicesInvocationTest.serviceConfiguration,
                        OPERATION_DEMANDERCONGES, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                        new ByteArrayInputStream(this.toByteArray(request))));

        // Assert the response of the request
        final ResponseMessage responseMsg = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs.get(0)),
                monitLogs.get(1));

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNotNull("No fault returns", fault);
        final String faultStr = SourceHelper.toString(fault);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg.getPayload());

        assertEquals(currentProcInstNb, this.getProcessInstanceNumber(BPMN_PROCESS_DEFINITION_KEY));
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the user identifier is missing.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request:
     * <ul>
     * <li>a fault is returned</li>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_NoUserIdValue() throws Exception {

        this.initIdentities();

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_1.size());
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnServicesInvocationTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        // Assert that the process instance is created
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_2.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());

        // Assert that the process instance and current user task
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the user identifier is empty.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request:
     * <ul>
     * <li>a fault is returned</li>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_EmptyUserIdValue() throws Exception {

        this.initIdentities();

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_1.size());
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnServicesInvocationTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        // Assert that the process instance is created
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("");
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_2.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());

        // Assert that the process instance and current user task
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the process instance identifier is missing.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request:
     * <ul>
     * <li>a fault is returned</li>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_NoProcessInstanceIdValue() throws Exception {

        this.initIdentities();

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_1.size());
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnServicesInvocationTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        // Assert that the process instance is created
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("valideur");
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_2.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault: " + faultStr, faultStr.contains(NoProcessInstanceIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());

        // Assert that the process instance and current user task
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the process instance identifier is empty.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request:
     * <ul>
     * <li>a fault is returned</li>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_EmptyProcessInstanceIdValue() throws Exception {

        this.initIdentities();

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_1.size());
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnServicesInvocationTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        // Assert that the process instance is created
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("demandeur");
        request_2.setNumeroDde("");
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_2.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault: " + faultStr, faultStr.contains(NoProcessInstanceIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());

        // Assert that the process instance and current user task
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a valid request is sent to complete a waiting user task where the process instance identifier does not exist
     * on the BPMN engine side.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>the right business fault associated to a process instance identifier not found is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_ProcessInstanceIdNotFound() throws Exception {

        // Create the valid request
        final Validation request = new Validation();
        request.setValideur("demandeur");
        final String unknownProcessInstanceId = "unknown-processInstanceId";
        request.setNumeroDde(unknownProcessInstanceId);
        request.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        BpmnServicesInvocationTest.componentUnderTest
                .pushRequestToProvider(new WrappedRequestToProviderMessage(
                        BpmnServicesInvocationTest.serviceConfiguration,
                        OPERATION_VALIDERDEMANDE, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                        new ByteArrayInputStream(this.toByteArray(request))));

        // Assert the response of the valid request
        final ResponseMessage responseMsg = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs.get(0)),
                monitLogs.get(1));

        // Check the reply
        assertNull("XML payload in response", responseMsg.getPayload());
        final Source fault = responseMsg.getFault();
        assertNotNull("No fault returns", fault);
        final Object responseObj = BpmnServicesInvocationTest.unmarshaller.unmarshal(fault);
        assertTrue(responseObj instanceof NumeroDemandeInconnu);
        final NumeroDemandeInconnu response = (NumeroDemandeInconnu) responseObj;
        assertEquals(unknownProcessInstanceId, response.getNumeroDde());
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd valid request is sent to complete the waiting user task,</li>
     * <li>a 3rd valid request is sent to complete again the user task already completed by the 2nd request.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request,:
     * <ul>
     * <li>the process instance is correctly finished in the Activiti engine,</li>
     * <li>the 1st user task is completed by the right assignee,</li>
     * <li>the service reply is as expected,</li>
     * <li>service tasks invoked Petals services</li>
     * </ul>
     * </li>
     * <li>on the 3rd request, the right business fault associated to a user task already completed is returned because
     * the user task is already completed.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_TaskCompletedFault() throws Exception {

        this.initIdentities();

        // Create the 1st valid request for start event 'request
        final Demande request_1 = new Demande();
        final String demandeur = "demandeur";
        request_1.setDemandeur(demandeur);
        final int numberOfDays = 10;
        request_1.setNbJourDde(numberOfDays);
        final GregorianCalendar now = new GregorianCalendar();
        final GregorianCalendar startDate = new GregorianCalendar(now.get(GregorianCalendar.YEAR),
                now.get(GregorianCalendar.MONTH), now.get(GregorianCalendar.DAY_OF_MONTH));
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(startDate));
        final String motivation = "hollidays";
        request_1.setMotifDde(motivation);

        // Send the 1st valid request for start event 'request
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_1.size());
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnServicesInvocationTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        // Check Activiti engine against the process instance creation
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Create the 2nd valid request for the user task 'handleRequest'
        final Validation request_2 = new Validation();
        final String valideur = "valideur";
        request_2.setValideur(valideur);
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.FALSE.toString());
        request_2.setMotifRefus("To not finished the process and be able to try to complete again the user task");

        // Send the 2nd valid request for the user task 'handleRequest
        inMemoryLogHandler.clear();
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_2.size());
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNull("Unexpected fault", (fault_2 == null ? null : SourceHelper.toString(fault_2)));
        assertNotNull("No XML payload in response", responseMsg_2.getPayload());
        final Object responseObj_2 = BpmnServicesInvocationTest.unmarshaller.unmarshal(responseMsg_2.getPayload());
        assertTrue(responseObj_2 instanceof AckResponse);
        final AckResponse response_2 = (AckResponse) responseObj_2;
        assertNotNull(response_2);

        // Check Activiti engine against the current user task
        assertProcessInstanceFinished(response_1.getNumeroDde());
        assertUserTaskEnded(response_1.getNumeroDde(), BPMN_PROCESS_2ND_USER_TASK_KEY, BPMN_USER_DEMANDEUR);

        // Create the 3rd request for the user task 'handleRequest'
        final Validation request_3 = new Validation();
        request_3.setValideur(valideur);
        request_3.setNumeroDde(response_1.getNumeroDde());
        request_3.setApprobation(Boolean.TRUE.toString());
        request_2.setMotifRefus("On this 2nd call a fault should occur completing the user task");

        // Send the 3rd valid request for the user task 'handleRequest'
        inMemoryLogHandler.clear();
        BpmnServicesInvocationTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnServicesInvocationTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_3))));

        // Assert the response of the 3rd valid request
        final ResponseMessage responseMsg_3 = BpmnServicesInvocationTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_3 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_3.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_3.get(0)),
                monitLogs_3.get(1));

        // Check the reply
        assertNull("XML payload in response", responseMsg_3.getPayload());
        final Source fault_3 = responseMsg_3.getFault();
        assertNotNull("No fault returns", fault_3);
        final Object responseObj_3 = BpmnServicesInvocationTest.unmarshaller.unmarshal(fault_3);
        assertTrue(responseObj_3 instanceof DemandeDejaValidee);
        final DemandeDejaValidee response_3 = (DemandeDejaValidee) responseObj_3;
        assertEquals(response_1.getNumeroDde(), response_3.getNumeroDde());

        // Check Activiti engine against the current user task
        assertProcessInstanceFinished(response_1.getNumeroDde());
        assertUserTaskEnded(response_1.getNumeroDde(), BPMN_PROCESS_2ND_USER_TASK_KEY, BPMN_USER_DEMANDEUR);
    }

}
