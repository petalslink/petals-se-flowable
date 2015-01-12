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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.ow2.petals.component.framework.junit.Assert.assertMonitProviderBeginLog;
import static org.ow2.petals.component.framework.junit.Assert.assertMonitProviderEndLog;
import static org.ow2.petals.component.framework.junit.Assert.assertMonitProviderFailureLog;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import javax.jbi.messaging.ExchangeStatus;
import javax.xml.bind.DatatypeConverter;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.activitibpmn.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.activitibpmn.operation.exception.NoUserIdValueException;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.Component;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.impl.ComponentConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration.ServiceType;
import org.ow2.petals.component.framework.junit.impl.message.WrappedRequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.WrappedStatusFromConsumerMessage;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTestBuilder;
import org.ow2.petals.junit.rules.log.handler.InMemoryLogHandler;
import org.ow2.petals.se.activitibpmn._1_0.su.AckResponse;
import org.ow2.petals.se.activitibpmn._1_0.su.Demande;
import org.ow2.petals.se.activitibpmn._1_0.su.DemandeDejaValidee;
import org.ow2.petals.se.activitibpmn._1_0.su.Numero;
import org.ow2.petals.se.activitibpmn._1_0.su.NumeroDemandeInconnu;
import org.ow2.petals.se.activitibpmn._1_0.su.Validation;
import org.ow2.petals.se.activitibpmn._1_0.su.XslParameter;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;
import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class BpmnComponentTest {

    private static final QName INTERFACE = new QName("http://petals.ow2.org/samples/se-bpmn", "demandeDeConges");

    private static final QName SERVICE = new QName("http://petals.ow2.org/samples/se-bpmn", "demandeDeCongesService");

    private static final String ENDPOINT = "testEndpointName";

    private static final QName OPERATION_DEMANDERCONGES = new QName("http://petals.ow2.org/samples/se-bpmn",
            "demanderConges");

    private static final QName OPERATION_VALIDERDEMANDE = new QName("http://petals.ow2.org/samples/se-bpmn",
            "validerDemande");

    @ClassRule
    public static final ComponentUnderTestBuilder componentBuilder = new ComponentUnderTestBuilder();

    @ClassRule
    public static final InMemoryLogHandler inMemoryLogHandler = new InMemoryLogHandler();

    private static Marshaller marshaller;

    private static Unmarshaller unmarshaller;

    private static Component componentUnderTest;

    private static ServiceConfiguration serviceConfiguration;

    static {
        try {
            JAXBContext context = JAXBContext.newInstance(Demande.class, Validation.class, Numero.class,
                    AckResponse.class, NumeroDemandeInconnu.class, DemandeDejaValidee.class);
            BpmnComponentTest.unmarshaller = context.createUnmarshaller();
            BpmnComponentTest.marshaller = context.createMarshaller();
            BpmnComponentTest.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    @BeforeClass
    public static void setup() throws Exception {
        BpmnComponentTest.initializeLoggingSystem();
        BpmnComponentTest.initializeAnsStartComponentUnderTest();
        BpmnComponentTest.deployServiceUnits();
    }

    @AfterClass
    public static void shutdown() {
        BpmnComponentTest.componentUnderTest.uninstallAllServices();
        BpmnComponentTest.componentUnderTest.shutdown();
    }

    /**
     * All log traces must be cleared before starting a unit test
     */
    @Before
    public void clearLogTraces() {
        inMemoryLogHandler.clear();
    }

    /**
     * Initialize the logging system reading the configuration defined in resource '/logging.properties'.
     */
    private static void initializeLoggingSystem() throws SecurityException, IOException {
        final InputStream inputStream = ActivitiSuManagerTest.class.getResourceAsStream("/logging.properties");
        assertNotNull("Logging configuration file not found", inputStream);
        try {
            LogManager.getLogManager().readConfiguration(inputStream);
        } finally {
            inputStream.close();
        }
    }

    /**
     * <p>
     * Initialize and start the component under test.
     * </p>
     * <p>
     * The configuration of the component is its default configuration, except the log level set to {@link Leve#FINE}.
     * </p>
     */
    private static void initializeAnsStartComponentUnderTest() throws Exception {

        final Logger logger = Logger.getAnonymousLogger();
        logger.addHandler(BpmnComponentTest.inMemoryLogHandler.getHandler());
        logger.setLevel(Level.FINE);
        BpmnComponentTest.componentUnderTest = BpmnComponentTest.componentBuilder.create(new ComponentConfiguration(
                "componentUnderTest", logger));
        BpmnComponentTest.componentUnderTest.start();
    }

    /**
     * Deploy service units
     */
    private static void deployServiceUnits() {

        final URL wsdlUrl = Thread.currentThread().getContextClassLoader().getResource("su/valid/vacationRequest.wsdl");
        assertNotNull("WSDl not found", wsdlUrl);
        BpmnComponentTest.serviceConfiguration = new ServiceConfiguration(BpmnComponentTest.class.getSimpleName(),
                INTERFACE, SERVICE, ENDPOINT, ServiceType.PROVIDE, wsdlUrl);

        final URL demanderCongesResponseXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/demanderCongesResponse.xsl");
        assertNotNull("Output XSL 'demanderCongesResponse.xsl' not found", demanderCongesResponseXslUrl);
        BpmnComponentTest.serviceConfiguration.addResource(demanderCongesResponseXslUrl);

        final URL validerDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/validerDemandeResponse.xsl");
        assertNotNull("Output XSL 'validerDemandeResponse.xsl' not found", validerDemandeResponseXslUrl);
        BpmnComponentTest.serviceConfiguration.addResource(validerDemandeResponseXslUrl);

        final URL ajusterDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/ajusterDemandeResponse.xsl");
        assertNotNull("Output XSL 'ajusterDemandeResponse.xsl' not found", ajusterDemandeResponseXslUrl);
        BpmnComponentTest.serviceConfiguration.addResource(ajusterDemandeResponseXslUrl);

        final URL numeroDemandeInconnuXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/numeroDemandeInconnu.xsl");
        assertNotNull("Output XSL 'numeroDemandeInconnu.xsl' not found", numeroDemandeInconnuXslUrl);
        BpmnComponentTest.serviceConfiguration.addResource(numeroDemandeInconnuXslUrl);

        final URL demandeDejaValideeXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/demandeDejaValidee.xsl");
        assertNotNull("Output XSL 'demandeDejaValidee.xsl' not found", demandeDejaValideeXslUrl);
        BpmnComponentTest.serviceConfiguration.addResource(demandeDejaValideeXslUrl);

        final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/vacationRequest.bpmn20.xml");
        assertNotNull("BPMN file not found", bpmnUrl);
        BpmnComponentTest.serviceConfiguration.addResource(bpmnUrl);
        BpmnComponentTest.serviceConfiguration.setParameter(
                "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}process_file",
                "vacationRequest.bpmn20.xml");
        BpmnComponentTest.serviceConfiguration.setParameter(
                "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}version", "1");

        BpmnComponentTest.componentUnderTest.installService(BpmnComponentTest.serviceConfiguration);
    }

    /**
     * <p>
     * Check the processing of the component when receiving the end (status 'DONE') of an IN_OU exchange.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no MONIT trace logged</li>
     * </ul>
     * </p>
     */
    @Test
    public void onDoneStatusAsProvider() {

        // Send an exchange with the status set to 'DONE'. We must use 'processMessageFromServiceBus' because nothing is
        // returned on the end of IN-OUT exchange
        BpmnComponentTest.componentUnderTest.processMessageFromServiceBus(new WrappedStatusFromConsumerMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "demanderConges"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream("".getBytes()), new FlowAttributes("testFlowInstanceId", "testFlowStepId"),
                ExchangeStatus.DONE));

        assertTrue(inMemoryLogHandler.getAllRecords(Level.MONIT).size() == 0);
    }

    /**
     * <p>
     * Check the processing of the component when receiving a IN-OUT message exchange with status 'ERROR'.
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no MONIT trace logged</li>
     * <li>a warning is logged telling that the message exchange is discarded.</li>
     * </ul>
     * </p>
     */
    @Test
    public void onErrorStatusAsProvider() {

        // Send an exchange with the status set to 'ERROR'. We must use 'processMessageFromServiceBus' because nothing
        // is
        // returned on the end of IN-OUT exchange
        BpmnComponentTest.componentUnderTest.processMessageFromServiceBus(new WrappedStatusFromConsumerMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "demanderConges"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream("".getBytes()), new FlowAttributes("testFlowInstanceId", "testFlowStepId"),
                ExchangeStatus.ERROR));

        assertTrue(inMemoryLogHandler.getAllRecords(Level.MONIT).size() == 0);
        assertTrue(inMemoryLogHandler.getAllRecords(Level.WARNING).size() == 1);
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
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, the user task is ended,</li>
     * <li>on the 3rd request, the right business fault associated to a user task already completed is returned because
     * the process instance is finished.</li>
     * </ul>
     * </p>
     */
    @Test
    public void validStartEventRequest() throws Exception {

        // Create the 1st valid request
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

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_1.size() == 2);
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
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
                    assertEquals(demandeur, xslParameter.getValue());
                } else if ("employeeName".equals(xslParameter.getName().toString())) {
                    employeeFound = true;
                    assertEquals(demandeur, xslParameter.getValue());
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
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        final String valideur = "valideur";
        request_2.setValideur(valideur);
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_2.size() == 2);
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNull("Unexpected fault", (fault_2 == null ? null : SourceHelper.toString(fault_2)));
        assertNotNull("No XML payload in response", responseMsg_2.getPayload());
        final Object responseObj_2 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_2.getPayload());
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
                    assertEquals(valideur, xslParameter.getValue());
                } else if ("employeeName".equals(xslParameter.getName().toString())) {
                    employeeFound = true;
                    assertEquals(demandeur, xslParameter.getValue());
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
        // TODO: Add a check against Activiti to verify that the process instance is finished

        // Create the 3rd request
        final Validation request_3 = new Validation();
        request_3.setValideur(valideur);
        request_3.setNumeroDde(response_1.getNumeroDde());
        request_3.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_3))));

        // Assert the response of the 3rd valid request
        final ResponseMessage responseMsg_3 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_3 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_3.size() == 2);
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_3.get(0)),
                monitLogs_3.get(1));

        // Check the reply
        assertNull("XML payload in response", responseMsg_3.getPayload());
        final Source fault_3 = responseMsg_3.getFault();
        assertNotNull("No fault returns", fault_3);
        final Object responseObj_3 = BpmnComponentTest.unmarshaller.unmarshal(fault_3);
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
     * </ul>
     * </p>
     */
    @Test
    public void startEventRequest_NoUserIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request = new Demande();
        request.setNbJourDde(10);
        request.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request.setMotifDde("hollidays");

        // Send the request
        BpmnComponentTest.componentUnderTest
                .pushRequestToProvider(new WrappedRequestToProviderMessage(BpmnComponentTest.serviceConfiguration,
                        OPERATION_DEMANDERCONGES, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                        new ByteArrayInputStream(this.toByteArray(request))));

        // Assert the response of the request
        final ResponseMessage responseMsg = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs.size() == 2);
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_DEMANDERCONGES, monitLogs.get(0)),
                monitLogs.get(1));

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNotNull("No fault returns", fault);
        final String faultStr = SourceHelper.toString(fault);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg.getPayload());
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

        // Create the 1st valid request
        final Demande request = new Demande();
        request.setDemandeur("");
        request.setNbJourDde(10);
        request.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request.setMotifDde("hollidays");

        // Send the request
        BpmnComponentTest.componentUnderTest
                .pushRequestToProvider(new WrappedRequestToProviderMessage(BpmnComponentTest.serviceConfiguration,
                        OPERATION_DEMANDERCONGES, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                        new ByteArrayInputStream(this.toByteArray(request))));

        // Assert the response of the request
        final ResponseMessage responseMsg = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs.size() == 2);
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_DEMANDERCONGES, monitLogs.get(0)),
                monitLogs.get(1));

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNotNull("No fault returns", fault);
        final String faultStr = SourceHelper.toString(fault);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg.getPayload());
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
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, a fault is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_NoUserIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_1.size() == 2);
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_2.size() == 2);
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());
        // TODO: Add a check against Activiti to verify that the user task is not complete
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
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, a fault is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_EmptyUserIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_1.size() == 2);
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("");
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_2.size() == 2);
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());
        // TODO: Add a check against Activiti to verify that the user task is not complete
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
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, a fault is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_NoProcessInstanceIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_1.size() == 2);
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("valideur");
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_2.size() == 2);
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault: " + faultStr, faultStr.contains(NoProcessInstanceIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());
        // TODO: Add a check against Activiti to verify that the user task is not complete
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
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, a fault is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_EmptyProcessInstanceIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_1.size() == 2);
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("demandeur");
        request_2.setNumeroDde("");
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_2.size() == 2);
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault: " + faultStr, faultStr.contains(NoProcessInstanceIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());
        // TODO: Add a check against Activiti to verify that the user task is not complete
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a valid request is sent to complete the waiting user task where the process instance identifier does not
     * exist on the BPMN engine side.</li>
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
        BpmnComponentTest.componentUnderTest
                .pushRequestToProvider(new WrappedRequestToProviderMessage(BpmnComponentTest.serviceConfiguration,
                        OPERATION_VALIDERDEMANDE, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                        new ByteArrayInputStream(this.toByteArray(request))));

        // Assert the response of the valid request
        final ResponseMessage responseMsg = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs.size() == 2);
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs.get(0)),
                monitLogs.get(1));

        // Check the reply
        assertNull("XML payload in response", responseMsg.getPayload());
        final Source fault = responseMsg.getFault();
        assertNotNull("No fault returns", fault);
        final Object responseObj = BpmnComponentTest.unmarshaller.unmarshal(fault);
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
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, the user task is ended,</li>
     * <li>on the 3rd request, the right business fault associated to a user task already completed is returned because
     * the user task is already completed.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_TaskCompletedFault() throws Exception {

        // Create the 1st valid request
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

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_1))));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_1.size() == 2);
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_DEMANDERCONGES, monitLogs_1.get(0)),
                monitLogs_1.get(1));

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        final String valideur = "valideur";
        request_2.setValideur(valideur);
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.FALSE.toString());
        request_2.setMotifRefus("To not finished the process and be able to try to complete again the user task");

        // Send the 2nd valid request
        inMemoryLogHandler.clear();
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_2))));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_2.size() == 2);
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)),
                monitLogs_2.get(1));

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNull("Unexpected fault", (fault_2 == null ? null : SourceHelper.toString(fault_2)));
        assertNotNull("No XML payload in response", responseMsg_2.getPayload());
        final Object responseObj_2 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_2.getPayload());
        assertTrue(responseObj_2 instanceof AckResponse);
        final AckResponse response_2 = (AckResponse) responseObj_2;
        // TODO: Add a check against Activiti to verify that the process instance is not finished

        // Create the 3rd request
        final Validation request_3 = new Validation();
        request_3.setValideur(valideur);
        request_3.setNumeroDde(response_1.getNumeroDde());
        request_3.setApprobation(Boolean.TRUE.toString());

        // Send the 3rd valid request
        inMemoryLogHandler.clear();
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), new ByteArrayInputStream(this
                        .toByteArray(request_3))));

        // Assert the response of the 3rd valid request
        final ResponseMessage responseMsg_3 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        // Check MONIT traces
        final List<LogRecord> monitLogs_3 = inMemoryLogHandler.getAllRecords(Level.MONIT);
        assertTrue(monitLogs_3.size() == 2);
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(INTERFACE, SERVICE, ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_3.get(0)),
                monitLogs_3.get(1));

        // Check the reply
        assertNull("XML payload in response", responseMsg_3.getPayload());
        final Source fault_3 = responseMsg_3.getFault();
        assertNotNull("No fault returns", fault_3);
        final Object responseObj_3 = BpmnComponentTest.unmarshaller.unmarshal(fault_3);
        assertTrue(responseObj_3 instanceof DemandeDejaValidee);
        final DemandeDejaValidee response_3 = (DemandeDejaValidee) responseObj_3;
        assertEquals(response_1.getNumeroDde(), response_3.getNumeroDde());
    }

    /**
     * Convert a JAXB element to bytes
     * 
     * @param jaxbElement
     *            The JAXB element to write as bytes
     */
    private byte[] toByteArray(final Object jaxbElement) throws JAXBException, IOException {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            BpmnComponentTest.marshaller.marshal(jaxbElement, baos);
            return baos.toByteArray();
        } finally {
            baos.close();
        }
    }

}
